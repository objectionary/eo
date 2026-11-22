/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang.sys;

import com.sun.jna.Pointer;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;
import org.eolang.ExFailure;

/**
 * Native pointers the runtime keeps on behalf of EO.
 *
 * <p>A directory stream is a pointer: {@code DIR*} on posix, a search
 * {@code HANDLE} on Windows. EO counts in doubles, which name no address
 * safely, so no pointer ever crosses into it. The pointer stays here instead,
 * under a small number that EO carries around and hands back to the call that
 * reads the stream or closes it, exactly the way it carries a file descriptor.
 * A number naming nothing is refused right here, rather than reaching the C
 * library as an address and taking the JVM down with it.</p>
 *
 * <p>Public because the syscall adapters live in other packages, and both of
 * them, posix and win32, hold their streams here.</p>
 *
 * @since 0.76.0
 */
public final class Handles {

    /**
     * Singleton.
     */
    public static final Handles INSTANCE = new Handles();

    /**
     * What is said about a number that stands for no pointer.
     */
    private static final String NAMES_NONE = "%s must be an open handle, but %d names none";

    /**
     * The pointers being kept, by the number standing for each one.
     */
    private final ConcurrentMap<Integer, Pointer> kept;

    /**
     * The number the next pointer is given.
     */
    private final AtomicInteger next;

    /**
     * Ctor.
     */
    private Handles() {
        this.kept = new ConcurrentHashMap<>(0);
        this.next = new AtomicInteger();
    }

    /**
     * Start keeping a pointer, under a number of its own.
     *
     * @param pointer The pointer to keep
     * @return The number standing for it
     */
    public int add(final Pointer pointer) {
        final int handle = this.next.incrementAndGet();
        this.kept.put(handle, pointer);
        return handle;
    }

    /**
     * The pointer a number stands for.
     *
     * @param subject What the number is, for the failure message
     * @param handle The number
     * @return The pointer
     */
    public Pointer get(final String subject, final int handle) {
        final Pointer pointer = this.kept.get(handle);
        if (pointer == null) {
            throw new ExFailure(Handles.NAMES_NONE, subject, handle);
        }
        return pointer;
    }

    /**
     * Stop keeping the pointer a number stands for, and give it back.
     *
     * <p>The pointer is taken out in one operation, so that two callers
     * closing the same handle at once never both receive it and close it
     * twice, which is undefined behaviour in C.</p>
     *
     * @param subject What the number is, for the failure message
     * @param handle The number
     * @return The pointer
     */
    public Pointer remove(final String subject, final int handle) {
        return Optional.ofNullable(this.kept.remove(handle)).orElseThrow(
            () -> new ExFailure(Handles.NAMES_NONE, subject, handle)
        );
    }
}
