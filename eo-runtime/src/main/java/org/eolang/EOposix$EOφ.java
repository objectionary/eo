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
import org.eolang.Posix.AcceptSyscall;
import org.eolang.Posix.AccessSyscall;
import org.eolang.Posix.BindSyscall;
import org.eolang.Posix.CloseSyscall;
import org.eolang.Posix.ConnectSyscall;
import org.eolang.Posix.CreatSyscall;
import org.eolang.Posix.ErrnoSyscall;
import org.eolang.Posix.GetenvSyscall;
import org.eolang.Posix.GetpidSyscall;
import org.eolang.Posix.GettimeofdaySyscall;
import org.eolang.Posix.InetAddrSyscall;
import org.eolang.Posix.ListenSyscall;
import org.eolang.Posix.MkdirSyscall;
import org.eolang.Posix.OpenSyscall;
import org.eolang.Posix.ReadSyscall;
import org.eolang.Posix.RecvSyscall;
import org.eolang.Posix.RenameSyscall;
import org.eolang.Posix.RmdirSyscall;
import org.eolang.Posix.SendSyscall;
import org.eolang.Posix.SocketSyscall;
import org.eolang.Posix.StatSyscall;
import org.eolang.Posix.StrerrorSyscall;
import org.eolang.Posix.UnlinkSyscall;
import org.eolang.Posix.WriteSyscall;

/**
 * Posix syscall.
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
 * @checkstyle ClassFanOutComplexityCheck (100 lines)
 */
@XmirObject(oname = "posix.@")
@SuppressWarnings("PMD.AvoidDollarSigns")
public final class EOposix$EOφ extends PhDefault implements Atom {

    /**
     * System calls map.
     */
    static final Map<String, Function<Phi, Syscall>> SYS_CALLS = new HashMap<>();

    static {
        EOposix$EOφ.SYS_CALLS.put("getpid", GetpidSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("open", OpenSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("access", AccessSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("stat", StatSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("creat", CreatSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("unlink", UnlinkSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("rmdir", RmdirSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("mkdir", MkdirSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("rename", RenameSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("read", ReadSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("write", WriteSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("getenv", GetenvSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("gettimeofday", GettimeofdaySyscall::new);
        EOposix$EOφ.SYS_CALLS.put("socket", SocketSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("close", CloseSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("connect", ConnectSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("bind", BindSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("listen", ListenSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("accept", AcceptSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("recv", RecvSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("send", SendSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("inet_addr", InetAddrSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("errno", ErrnoSyscall::new);
        EOposix$EOφ.SYS_CALLS.put("strerror", StrerrorSyscall::new);
    }

    @Override
    public Phi lambda() {
        final Phi rho = this.take(Phi.RHO);
        final String call = new Dataized(rho.take("name")).asString();
        if (!EOposix$EOφ.SYS_CALLS.containsKey(call)) {
            throw new ExFailure(
                "Can't make posix syscall '%s' because it's either not supported yet or does not exist",
                call
            );
        }
        return EOposix$EOφ.SYS_CALLS.get(call).apply(rho).make(
            new TupleToArray(rho.take("args")).get()
        );
    }
}
