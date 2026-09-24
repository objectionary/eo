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
import org.eolang.sys.Syscall;

/**
 * A POSIX syscall known by its name.
 *
 * <p>The name arrives from EO as a string, which this object turns into
 * the syscall that carries it out.</p>
 *
 * @since 0.62.0
 */
public final class NamedSyscall implements Syscall {

    /**
     * All syscalls, by their POSIX names.
     */
    private static final Map<String, Function<Phi, Syscall>> ALL = new HashMap<>();

    static {
        NamedSyscall.ALL.put("open", OpenSyscall::new);
        NamedSyscall.ALL.put("opendir", OpendirSyscall::new);
        NamedSyscall.ALL.put("readdir", ReaddirSyscall::new);
        NamedSyscall.ALL.put("closedir", ClosedirSyscall::new);
        NamedSyscall.ALL.put("unlink", UnlinkSyscall::new);
        NamedSyscall.ALL.put("rmdir", RmdirSyscall::new);
        NamedSyscall.ALL.put("mkdir", MkdirSyscall::new);
        NamedSyscall.ALL.put("rename", RenameSyscall::new);
        NamedSyscall.ALL.put("symlink", SymlinkSyscall::new);
        NamedSyscall.ALL.put("getenv", GetenvSyscall::new);
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
     *
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
