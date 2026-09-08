/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

/**
 * An object remembering the results of its own dataization (see #5165).
 *
 * <p>The transpiler puts this decorator on a formation that {@code purify.xsl}
 * marked as pure: one whose answer is decided by the bytes of its
 * inputs and by nothing else. The cache belongs to the decorated object alone
 * and holds only what is its own — there is no table shared between objects
 * and no name under which a stranger could find an entry. A copy shares the
 * cache of its origin, because an application copies the formation before
 * filling it, and a cache that died with every copy would never answer
 * anything.</p>
 *
 * <p>The inputs are whatever is put into the object — a void, a receiver, it
 * makes no difference — and the key is those puts in the order they came,
 * each reduced to the bytes of its object. A put is remembered under the slot
 * it came through, so the same void filled by position once and by name later
 * makes two entries: a second computation, never a wrong answer. The label of
 * the transpiler is a promise about the callers of yesterday and not about
 * those of tomorrow, so before trusting an input the decorator resolves it
 * and looks at what it is: anything that is not a number or a string makes
 * the whole dataization pass through, computed and not remembered.</p>
 *
 * <p>Only dataization is remembered. Taking an attribute, putting one,
 * copying, normalizing — all of it reaches the decorated object untouched,
 * and the cache is bounded, letting the entry asked for longest ago go
 * first.</p>
 *
 * <p>An object of this class is equal to itself and to nothing else, the
 * way {@code PhDefault} is. Answering on behalf of the decorated object
 * would make the answer one-sided: the decorator would say it equals the
 * object it wraps, while that object says it does not equal the decorator,
 * and {@code Object.equals} requires the two to agree.</p>
 *
 * @since 0.75
 */
public final class PhSticky implements Phi {

    /**
     * The formae whose objects are decided by their bytes alone.
     */
    private static final List<String> DATA = List.of(
        String.join(".", PhPackage.GLOBAL, "number"),
        String.join(".", PhPackage.GLOBAL, "string"),
        String.join(".", PhPackage.GLOBAL, "bytes")
    );

    /**
     * The object decorated.
     */
    private final Phi origin;

    /**
     * The answers remembered so far, shared with every copy.
     */
    private final Map<String, byte[]> cache;

    /**
     * The puts this copy has received, in order.
     */
    private final List<Map.Entry<String, Phi>> inputs;

    /**
     * What a caller holds while it computes an answer, by key, shared with
     * every copy, so that two callers asking for the same answer wait for
     * one computation instead of running two. An entry lives no longer than
     * the computation it guards.
     */
    private final Map<String, Lock> guards;

    /**
     * Ctor.
     * @param obj The object to decorate
     */
    public PhSticky(final Phi obj) {
        this(obj, 256);
    }

    /**
     * Ctor.
     * @param obj The object to decorate
     * @param capacity How many answers to keep before evicting
     */
    public PhSticky(final Phi obj, final int capacity) {
        this(
            obj,
            Collections.synchronizedMap(new Lru<>(capacity)),
            new CopyOnWriteArrayList<>(),
            new ConcurrentHashMap<>(0)
        );
    }

    /**
     * Primary ctor.
     * @param obj The object to decorate
     * @param map The answers remembered so far
     * @param puts The puts received so far
     * @param busy What a caller holds while it computes an answer
     */
    private PhSticky(
        final Phi obj,
        final Map<String, byte[]> map,
        final List<Map.Entry<String, Phi>> puts,
        final Map<String, Lock> busy
    ) {
        this.origin = obj;
        this.cache = map;
        this.inputs = puts;
        this.guards = busy;
    }

    @Override
    public boolean equals(final Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return System.identityHashCode(this) + 1;
    }

    @Override
    public Phi copy() {
        return new PhSticky(
            this.origin.copy(),
            this.cache,
            new CopyOnWriteArrayList<>(this.inputs),
            this.guards
        );
    }

    @Override
    public boolean needsRho() {
        return this.origin.needsRho();
    }

    @Override
    public Phi take(final String name) {
        return this.origin.take(name);
    }

    @Override
    public void put(final int pos, final Phi object) {
        this.origin.put(pos, object);
        this.inputs.add(Map.entry(String.valueOf(pos), object));
    }

    @Override
    public void put(final String name, final Phi object) {
        this.origin.put(name, object);
        this.inputs.add(Map.entry(name, object));
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }

    @Override
    public byte[] delta() {
        final byte[] result;
        final Optional<String> key = this.key();
        if (key.isPresent()) {
            result = this.remembered(key.get());
        } else {
            result = this.origin.delta();
        }
        return result;
    }

    @Override
    public Phi normalized() {
        Phi result = this.origin.normalized();
        if (result.equals(this.origin)) {
            result = this;
        }
        return result;
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    private byte[] remembered(final String key) {
        final byte[] result;
        final Lock guard = this.guards.computeIfAbsent(key, ignored -> new ReentrantLock());
        guard.lock();
        try {
            final byte[] found = this.cache.get(key);
            if (found == null) {
                result = this.origin.delta();
                this.cache.put(key, result.clone());
            } else {
                result = found;
            }
        } finally {
            this.guards.remove(key, guard);
            guard.unlock();
        }
        return result.clone();
    }

    private Optional<String> key() {
        final Optional<String> result;
        final List<Map.Entry<String, Phi>> normal = this.inputs.stream()
            .map(put -> Map.entry(put.getKey(), put.getValue().normalized()))
            .collect(Collectors.toList());
        if (normal.stream().allMatch(put -> PhSticky.DATA.contains(put.getValue().forma()))) {
            result = Optional.of(
                normal.stream().map(
                    put -> String.format(
                        "%s=%s:%s",
                        put.getKey(),
                        put.getValue().forma(),
                        Base64.getEncoder().encodeToString(
                            new Dataized(put.getValue()).take()
                        )
                    )
                ).collect(Collectors.joining("|"))
            );
        } else {
            result = Optional.empty();
        }
        return result;
    }
}
