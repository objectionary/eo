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
 * A Winsock function call known by its name, split out of {@link NamedFuncCall}
 * to keep that class's fan-out down: sockets stay on Winsock (ws2_32), unlike
 * every other Win32 call this package makes, which goes through the C runtime.
 *
 * @since 0.75.0
 */
final class NamedSocketFuncCall implements Syscall {

    /**
     * All function calls, by their Winsock names.
     */
    private static final Map<String, Function<Phi, Syscall>> ALL = new HashMap<>();

    static {
        NamedSocketFuncCall.ALL.put("WSAStartup", WSAStartupFuncCall::new);
        NamedSocketFuncCall.ALL.put("WSACleanup", WSACleanupFuncCall::new);
        NamedSocketFuncCall.ALL.put("WSAGetLastError", WSAGetLastErrorFuncCall::new);
        NamedSocketFuncCall.ALL.put("socket", SocketFuncCall::new);
        NamedSocketFuncCall.ALL.put("connect", ConnectFuncCall::new);
        NamedSocketFuncCall.ALL.put("accept", AcceptFuncCall::new);
        NamedSocketFuncCall.ALL.put("bind", BindFuncCall::new);
        NamedSocketFuncCall.ALL.put("listen", ListenFuncCall::new);
        NamedSocketFuncCall.ALL.put("send", SendFuncCall::new);
        NamedSocketFuncCall.ALL.put("recv", RecvFuncCall::new);
        NamedSocketFuncCall.ALL.put("closesocket", ClosesocketFuncCall::new);
        NamedSocketFuncCall.ALL.put("inet_addr", InetAddrFuncCall::new);
    }

    /**
     * The Winsock name of the function.
     */
    private final String name;

    /**
     * The object the function call belongs to.
     */
    private final Phi rho;

    /**
     * Ctor.
     *
     * @param call The Winsock name of the function
     * @param obj The object the function call belongs to
     */
    NamedSocketFuncCall(final String call, final Phi obj) {
        this.name = call;
        this.rho = obj;
    }

    @Override
    public Phi make(final Phi... params) {
        if (!NamedSocketFuncCall.ALL.containsKey(this.name)) {
            throw new ExFailure(
                "Can't make win32 function call '%s' because it's either not supported yet or does not exist",
                this.name
            );
        }
        return NamedSocketFuncCall.ALL.get(this.name).apply(this.rho).make(params);
    }
}
