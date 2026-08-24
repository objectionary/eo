/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * An object remembering the results of its own dataization (see #5165).
 *
 * <p>The transpiler puts this decorator on a formation that {@code purify.xsl}
 * marked with {@code @pure}: one whose answer is decided by the bytes of its
 * inputs and by nothing else. The cache belongs to the decorated object alone
 * and holds only what is its own — there is no table shared between objects
 * and no name under which a stranger could find an entry. A copy shares the
 * cache of its origin, because an application copies the formation before
 * filling it, and a cache that died with every copy would never answer
 * anything.</p>
 *
 * <p>The inputs are whatever is put into the object — a void, a receiver, it
 * makes no difference — and the key is those puts in the order they came,
 * each reduced to the bytes of its object. The label of the transpiler is a
 * promise about the callers of yesterday and not about those of tomorrow, so
 * before trusting an input the decorator resolves it and looks at what it is:
 * anything that is not a number or a string makes the whole dataization pass
 * through, computed and not remembered.</p>
 *
 * <p>Only dataization is remembered. Taking an attribute, putting one,
 * copying, normalizing — all of it reaches the decorated object untouched,
 * and the cache is bounded, letting the entry asked for longest ago go first.</p>
 *
 * @since 0.75
 */
public final class PhSticky implements Phi {

    /**
     * The formae whose objects are decided by their bytes alone.
     */
    private static final List<String> DATA = List.of(
        String.join(".", PhPackage.GLOBAL, "number"),
        String.join(".", PhPackage.GLOBAL, "string")
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
            Collections.synchronizedMap(new Lru(capacity)),
            Collections.synchronizedList(new ArrayList<>(0))
        );
    }

    /**
     * Primary ctor.
     * @param obj The object to decorate
     * @param map The answers remembered so far
     * @param puts The puts received so far
     */
    private PhSticky(
        final Phi obj,
        final Map<String, byte[]> map,
        final List<Map.Entry<String, Phi>> puts
    ) {
        this.origin = obj;
        this.cache = map;
        this.inputs = puts;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.origin.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.origin.hashCode();
    }

    @Override
    public Phi copy() {
        final List<Map.Entry<String, Phi>> puts;
        synchronized (this.inputs) {
            puts = Collections.synchronizedList(new ArrayList<>(this.inputs));
        }
        return new PhSticky(this.origin.copy(), this.cache, puts);
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
        this.inputs.add(new AbstractMap.SimpleImmutableEntry<>(String.valueOf(pos), object));
    }

    @Override
    public void put(final String name, final Phi object) {
        this.origin.put(name, object);
        this.inputs.add(new AbstractMap.SimpleImmutableEntry<>(name, object));
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
            final byte[] found = this.cache.get(key.get());
            if (found == null) {
                result = this.origin.delta();
                this.cache.put(key.get(), result.clone());
            } else {
                result = found.clone();
            }
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

    /**
     * The key of this copy in the cache, or nothing when some input
     * is not data and the cache must stay out of the way.
     * @return The key, or empty
     */
    private Optional<String> key() {
        final List<Map.Entry<String, Phi>> puts;
        synchronized (this.inputs) {
            puts = new ArrayList<>(this.inputs);
        }
        final StringBuilder out = new StringBuilder(0);
        Optional<String> key = Optional.empty();
        boolean data = true;
        for (final Map.Entry<String, Phi> put : puts) {
            final Phi norm = put.getValue().normalized();
            if (!PhSticky.DATA.contains(norm.forma())) {
                data = false;
                break;
            }
            out.append(put.getKey()).append('=')
                .append(Base64.getEncoder().encodeToString(new Dataized(norm).take()))
                .append('|');
        }
        if (data) {
            key = Optional.of(out.toString());
        }
        return key;
    }
}
