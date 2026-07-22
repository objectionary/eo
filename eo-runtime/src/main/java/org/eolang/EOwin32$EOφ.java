/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import org.eolang.Win32.AcceptFuncCall;
import org.eolang.Win32.AccessFuncCall;
import org.eolang.Win32.BindFuncCall;
import org.eolang.Win32.CloseFuncCall;
import org.eolang.Win32.ClosesocketFuncCall;
import org.eolang.Win32.ConnectFuncCall;
import org.eolang.Win32.CreatFuncCall;
import org.eolang.Win32.FtimeFuncCall;
import org.eolang.Win32.GetenvFuncCall;
import org.eolang.Win32.GetpidFuncCall;
import org.eolang.Win32.InetAddrFuncCall;
import org.eolang.Win32.ListenFuncCall;
import org.eolang.Win32.MkdirFuncCall;
import org.eolang.Win32.OpenFuncCall;
import org.eolang.Win32.ReadFuncCall;
import org.eolang.Win32.RecvFuncCall;
import org.eolang.Win32.RenameFuncCall;
import org.eolang.Win32.RmdirFuncCall;
import org.eolang.Win32.SendFuncCall;
import org.eolang.Win32.SocketFuncCall;
import org.eolang.Win32.Stat64FuncCall;
import org.eolang.Win32.UnlinkFuncCall;
import org.eolang.Win32.WSACleanupFuncCall;
import org.eolang.Win32.WSAGetLastErrorFuncCall;
import org.eolang.Win32.WSAStartupFuncCall;
import org.eolang.Win32.WriteFuncCall;

/**
 * Win32 function call.
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 * @checkstyle ClassFanOutComplexityCheck (100 lines)
 */
@XmirObject(oname = "win32.@")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOwin32$EOφ extends PhDefault implements Atom {

    /**
     * Function calls map.
     */
    static final Map<String, Function<Phi, Syscall>> FUNCTIONS = new HashMap<>();

    static {
        EOwin32$EOφ.FUNCTIONS.put("_getpid", GetpidFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_open", OpenFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_access", AccessFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_stat64", Stat64FuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_creat", CreatFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_unlink", UnlinkFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_rmdir", RmdirFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_mkdir", MkdirFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("rename", RenameFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_read", ReadFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_write", WriteFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_close", CloseFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("getenv", GetenvFuncCall::new);
        EOwin32$EOφ.FUNCTIONS.put("_ftime32_s", FtimeFuncCall::new);
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
        final Phi rho = this.take(Phi.RHO);
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
