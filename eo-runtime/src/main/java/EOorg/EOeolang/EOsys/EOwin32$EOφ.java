/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys; // NOPMD

import EOorg.EOeolang.EOsys.Win32.AcceptFuncCall;
import EOorg.EOeolang.EOsys.Win32.BindFuncCall;
import EOorg.EOeolang.EOsys.Win32.ClosesocketFuncCall;
import EOorg.EOeolang.EOsys.Win32.ConnectFuncCall;
import EOorg.EOeolang.EOsys.Win32.GetCurrentProcessIdFuncCall;
import EOorg.EOeolang.EOsys.Win32.GetEnvironmentVariableFuncCall;
import EOorg.EOeolang.EOsys.Win32.GetSystemTimeFuncCall;
import EOorg.EOeolang.EOsys.Win32.InetAddrFuncCall;
import EOorg.EOeolang.EOsys.Win32.ListenFuncCall;
import EOorg.EOeolang.EOsys.Win32.ReadFileFuncCall;
import EOorg.EOeolang.EOsys.Win32.RecvFuncCall;
import EOorg.EOeolang.EOsys.Win32.SendFuncCall;
import EOorg.EOeolang.EOsys.Win32.SocketFuncCall;
import EOorg.EOeolang.EOsys.Win32.WSACleanupFuncCall;
import EOorg.EOeolang.EOsys.Win32.WSAGetLastErrorFuncCall;
import EOorg.EOeolang.EOsys.Win32.WSAStartupFuncCall;
import EOorg.EOeolang.EOsys.Win32.WriteFileFuncCall;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import org.eolang.Atom;
import org.eolang.Attr;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Win32 function call.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 */
@XmirObject(oname = "win32.@")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOφ extends PhDefault implements Atom {
    /**
     * Function calls map.
     */
    static final Map<String, Function<Phi, Syscall>> FUNCTIONS = new HashMap<>();

    static {
        EOwin32$EOφ.FUNCTIONS.put("GetCurrentProcessId", GetCurrentProcessIdFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("ReadFile", ReadFileFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("WriteFile", WriteFileFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("GetEnvironmentVariable", GetEnvironmentVariableFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("GetSystemTime", GetSystemTimeFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("WSAStartup", WSAStartupFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("WSACleanup", WSACleanupFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("WSAGetLastError", WSAGetLastErrorFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("socket", SocketFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("connect", ConnectFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("accept", AcceptFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("bind", BindFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("listen", ListenFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("send", SendFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("recv", RecvFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("closesocket", ClosesocketFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("inet_addr", InetAddrFuncCall::new);
    }

    @Override
    public Phi lambda() {
        final Phi rho = this.take(Attr.RHO);
        final String func = new Dataized(rho.take("name")).asString();
        if (!EOwin32$EOφ.FUNCTIONS.containsKey(func)) {
            throw new ExFailure(
                "Can't make win32 function call '%s' because it's either not supported yet or does not exist",
                func
            );
        }
        return EOwin32$EOφ.FUNCTIONS.get(func).apply(rho).make(
            new TupleToArray(rho.take("args")).get()
        );
    }
}
