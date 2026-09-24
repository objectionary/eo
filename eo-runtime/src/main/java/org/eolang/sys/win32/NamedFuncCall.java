/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.sys.win32;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.eolang.sys.Syscall;

/**
 * A Win32 function call known by its name.
 *
 * <p>The name arrives from EO as a string, which this object turns into
 * the function call that carries it out.</p>
 *
 * @since 0.62.0
 */
public final class NamedFuncCall implements Syscall {

    /**
     * All function calls, by their Win32 names.
     */
    private static final Map<String, Function<Phi, Syscall>> ALL = new HashMap<>();

    static {
        NamedFuncCall.ALL.put("FindFirstFileW", FindFirstFileFuncCall::new);
    }

    /**
     * The Win32 name of the function.
     */
    private final String name;

    /**
     * The object the function call belongs to.
     */
    private final Phi rho;

    /**
     * Ctor.
     *
     * @param name The Win32 name of the function
     * @param rho The object the function call belongs to
     */
    public NamedFuncCall(final String name, final Phi rho) {
        this.name = name;
        this.rho = rho;
    }

    @Override
    public Phi make(final Phi... params) {
        if (!NamedFuncCall.ALL.containsKey(this.name)) {
            throw new ExFailure(
                "Can't make win32 function call '%s' because it's either not supported yet or does not exist",
                this.name
            );
        }
        return NamedFuncCall.ALL.get(this.name).apply(this.rho).make(params);
    }
}
