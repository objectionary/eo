/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys; // NOPMD

import EOorg.EOeolang.EOsys.Posix.AcceptSyscall;
import EOorg.EOeolang.EOsys.Posix.BindSyscall;
import EOorg.EOeolang.EOsys.Posix.CloseSyscall;
import EOorg.EOeolang.EOsys.Posix.ConnectSyscall;
import EOorg.EOeolang.EOsys.Posix.ErrnoSyscall;
import EOorg.EOeolang.EOsys.Posix.GetenvSyscall;
import EOorg.EOeolang.EOsys.Posix.GetpidSyscall;
import EOorg.EOeolang.EOsys.Posix.GettimeofdaySyscall;
import EOorg.EOeolang.EOsys.Posix.InetAddrSyscall;
import EOorg.EOeolang.EOsys.Posix.ListenSyscall;
import EOorg.EOeolang.EOsys.Posix.ReadSyscall;
import EOorg.EOeolang.EOsys.Posix.RecvSyscall;
import EOorg.EOeolang.EOsys.Posix.SendSyscall;
import EOorg.EOeolang.EOsys.Posix.SocketSyscall;
import EOorg.EOeolang.EOsys.Posix.StrerrorSyscall;
import EOorg.EOeolang.EOsys.Posix.WriteSyscall;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import org.eolang.Atom;
import org.eolang.Dataized;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Posix syscall.
 *
 * @since 0.40
 * @checkstyle TypeNameCheck (100 lines)
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
