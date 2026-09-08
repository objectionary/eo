/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import org.eolang.Phi;
import org.eolang.Syscall;

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
        NamedFuncCall.ALL.put("_getpid", GetpidFuncCall::new);
        NamedFuncCall.ALL.put("_errno", ErrnoFuncCall::new);
        NamedFuncCall.ALL.put("_open", OpenFuncCall::new);
        NamedFuncCall.ALL.put("_access", AccessFuncCall::new);
        NamedFuncCall.ALL.put("_stat64", Stat64FuncCall::new);
        NamedFuncCall.ALL.put("GetFileAttributesW", GetFileAttributesFuncCall::new);
        NamedFuncCall.ALL.put("FindFirstFileW", FindFirstFileFuncCall::new);
        NamedFuncCall.ALL.put("FindNextFileW", FindNextFileFuncCall::new);
        NamedFuncCall.ALL.put("FindClose", FindCloseFuncCall::new);
        NamedFuncCall.ALL.put("_creat", CreatFuncCall::new);
        NamedFuncCall.ALL.put("_unlink", UnlinkFuncCall::new);
        NamedFuncCall.ALL.put("_rmdir", RmdirFuncCall::new);
        NamedFuncCall.ALL.put("_mkdir", MkdirFuncCall::new);
        NamedFuncCall.ALL.put("rename", RenameFuncCall::new);
        NamedFuncCall.ALL.put("_read", ReadFuncCall::new);
        NamedFuncCall.ALL.put("_write", WriteFuncCall::new);
        NamedFuncCall.ALL.put("_close", CloseFuncCall::new);
        NamedFuncCall.ALL.put("getenv", GetenvFuncCall::new);
        NamedFuncCall.ALL.put("_ftime64_s", FtimeFuncCall::new);
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
     * @param name The Win32 name of the function
     * @param rho The object the function call belongs to
     */
    public NamedFuncCall(final String name, final Phi rho) {
        this.name = name;
        this.rho = rho;
    }

    @Override
    public Phi make(final Phi... params) {
        final Syscall call;
        if (NamedFuncCall.ALL.containsKey(this.name)) {
            call = NamedFuncCall.ALL.get(this.name).apply(this.rho);
        } else {
            call = new NamedSocketFuncCall(this.name, this.rho);
        }
        return call.make(params);
    }
}
