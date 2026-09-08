/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import org.eolang.ExFailure;
import org.eolang.Phi;
import org.eolang.Syscall;

/**
 * A POSIX syscall known by its name.
 *
 * <p>The name arrives from EO as a string, which this object turns into
 * the syscall that carries it out.</p>
 *
 * @since 0.62.0
 * @checkstyle ClassFanOutComplexityCheck (100 lines)
 */
public final class NamedSyscall implements Syscall {

    /**
     * All syscalls, by their POSIX names.
     */
    private static final Map<String, Function<Phi, Syscall>> ALL = new HashMap<>();

    static {
        NamedSyscall.ALL.put("getpid", GetpidSyscall::new);
        NamedSyscall.ALL.put("open", OpenSyscall::new);
        NamedSyscall.ALL.put("access", AccessSyscall::new);
        NamedSyscall.ALL.put(
            "stat",
            posix -> new StatSyscall(posix, (path, buf) -> CStdLib.INSTANCE.stat(path, buf))
        );
        NamedSyscall.ALL.put(
            "lstat",
            posix -> new StatSyscall(posix, (path, buf) -> CStdLib.INSTANCE.lstat(path, buf))
        );
        NamedSyscall.ALL.put("opendir", OpendirSyscall::new);
        NamedSyscall.ALL.put("readdir", ReaddirSyscall::new);
        NamedSyscall.ALL.put("closedir", ClosedirSyscall::new);
        NamedSyscall.ALL.put("creat", CreatSyscall::new);
        NamedSyscall.ALL.put("unlink", UnlinkSyscall::new);
        NamedSyscall.ALL.put("rmdir", RmdirSyscall::new);
        NamedSyscall.ALL.put("mkdir", MkdirSyscall::new);
        NamedSyscall.ALL.put("rename", RenameSyscall::new);
        NamedSyscall.ALL.put("symlink", SymlinkSyscall::new);
        NamedSyscall.ALL.put("read", ReadSyscall::new);
        NamedSyscall.ALL.put("write", WriteSyscall::new);
        NamedSyscall.ALL.put("getenv", GetenvSyscall::new);
        NamedSyscall.ALL.put("gettimeofday", GettimeofdaySyscall::new);
        NamedSyscall.ALL.put("socket", SocketSyscall::new);
        NamedSyscall.ALL.put("close", CloseSyscall::new);
        NamedSyscall.ALL.put("connect", ConnectSyscall::new);
        NamedSyscall.ALL.put("bind", BindSyscall::new);
        NamedSyscall.ALL.put("listen", ListenSyscall::new);
        NamedSyscall.ALL.put("accept", AcceptSyscall::new);
        NamedSyscall.ALL.put("recv", RecvSyscall::new);
        NamedSyscall.ALL.put("send", SendSyscall::new);
        NamedSyscall.ALL.put("inet_addr", InetAddrSyscall::new);
        NamedSyscall.ALL.put("errno", ErrnoSyscall::new);
        NamedSyscall.ALL.put("strerror", StrerrorSyscall::new);
    }

    /**
     * The POSIX name of the syscall.
     */
    private final String name;

    /**
     * The object the syscall belongs to.
     */
    private final Phi rho;

    /**
     * Ctor.
     * @param name The POSIX name of the syscall
     * @param rho The object the syscall belongs to
     */
    public NamedSyscall(final String name, final Phi rho) {
        this.name = name;
        this.rho = rho;
    }

    @Override
    public Phi make(final Phi... params) {
        if (!NamedSyscall.ALL.containsKey(this.name)) {
            throw new ExFailure(
                "Can't make posix syscall '%s' because it's either not supported yet or does not exist",
                this.name
            );
        }
        return NamedSyscall.ALL.get(this.name).apply(this.rho).make(params);
    }
}
