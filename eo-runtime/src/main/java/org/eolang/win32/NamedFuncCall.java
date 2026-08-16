/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import org.eolang.ExFailure;
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
        NamedFuncCall.ALL.put("_open", OpenFuncCall::new);
        NamedFuncCall.ALL.put("_access", AccessFuncCall::new);
        NamedFuncCall.ALL.put("_stat64", Stat64FuncCall::new);
        NamedFuncCall.ALL.put("GetFileAttributesW", GetFileAttributesFuncCall::new);
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
        NamedFuncCall.ALL.put("WSAStartup", WSAStartupFuncCall::new);
        NamedFuncCall.ALL.put("WSACleanup", WSACleanupFuncCall::new);
        NamedFuncCall.ALL.put("WSAGetLastError", WSAGetLastErrorFuncCall::new);
        NamedFuncCall.ALL.put("socket", SocketFuncCall::new);
        NamedFuncCall.ALL.put("connect", ConnectFuncCall::new);
        NamedFuncCall.ALL.put("accept", AcceptFuncCall::new);
        NamedFuncCall.ALL.put("bind", BindFuncCall::new);
        NamedFuncCall.ALL.put("listen", ListenFuncCall::new);
        NamedFuncCall.ALL.put("send", SendFuncCall::new);
        NamedFuncCall.ALL.put("recv", RecvFuncCall::new);
        NamedFuncCall.ALL.put("closesocket", ClosesocketFuncCall::new);
        NamedFuncCall.ALL.put("inet_addr", InetAddrFuncCall::new);
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
        if (!NamedFuncCall.ALL.containsKey(this.name)) {
            throw new ExFailure(
                "Can't make win32 function call '%s' because it's either not supported yet or does not exist",
                this.name
            );
        }
        return NamedFuncCall.ALL.get(this.name).apply(this.rho).make(params);
    }
}
