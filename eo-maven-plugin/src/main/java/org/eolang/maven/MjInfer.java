/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import io.github.sekator778.seqdir.Seqdir;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Mapped;
import org.eolang.inference.Names;

/**
 * Mojo that reads what a program says about its own objects,
 * preferably after the {@code assemble} goal.
 *
 * <p>What a program says about its objects is written down into
 * {@code facts.xml}, which a later goal can read. In this program, for
 * example, {@code t} has {@code next}, the object attached to {@code next}
 * has nothing, and {@code inc} reads {@code foo} off whatever it is
 * given:</p>
 *
 * <pre> [] &gt; app
 *   inc t &gt; @
 *   [] &gt; t
 *     [] &gt; next
 *   [x] &gt; inc
 *     x.next.foo &gt; @</pre>
 *
 * <p>One pass through the XMIR of every file parsed so far writes every
 * formation with its attributes, every name read and what it is read off,
 * the head of every chain, and every call with its arguments.</p>
 *
 * <p>Nothing here says whether the program is wrong. Judging it was written
 * and taken out again in #6661, because a verdict needs the object that
 * misses an attribute to have been seen whole, and almost none of them have
 * been; the program has to be described before anything can read it for
 * mistakes.</p>
 *
 * <p>The XMIR of every registered source is read, and not every file the
 * parse directory holds: a source that was deleted leaves its XMIR behind,
 * and the facts would go on describing an object nobody wrote any more
 * (#9271). Where each one lies is worked out from the identifier of the
 * tojo, and not read off its {@code xmir} attribute, because {@code eo:merge}
 * moves that attribute of a package object into the merge directory, while
 * the facts are made of what the parser wrote.</p>
 *
 * <p>The engine then reads {@code facts.xml} and writes {@code answers.xml}
 * beside it: for every place, which objects it can be when Java runs the
 * program. It has to know three lists of names of the program, which are
 * parameters here: {@link #canon}, {@link #roots} and {@link #scalars}.
 * Almost every program is compiled together with eo-runtime, so a list
 * left unset or empty holds the names of eo-runtime (#10079), and a program
 * that has other names sets its own. Maven gives an empty list, not
 * {@code null}, to a list parameter nobody sets, so empty means unset here.
 * The names of eo-runtime do no harm to a program that has none of these
 * objects: the canon is made only when every kind of it is there, and the
 * other names then match nothing.</p>
 *
 * <p>The prepared XMIR is saved in {@link #prepared}, and {@code facts.xml}
 * and {@code answers.xml} in {@link #tables}. Nothing of it fails the
 * build.</p>
 *
 * @since 0.67.0
 */
@Mojo(
    name = "infer",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjInfer extends MjSafe {

    /**
     * The canon of eo-runtime, taken when {@link #canon} is unset
     * or empty.
     */
    private static final List<String> RUNTIME_CANON = List.of("Φ.bytes", "Φ.number", "Φ.string");

    /**
     * The packages of list code of eo-runtime, taken when {@link #roots}
     * is unset or empty.
     */
    private static final List<String> RUNTIME_ROOTS = List.of("Φ.tuple", "Φ.map", "Φ.set");

    /**
     * The scalar packages of eo-runtime, taken when {@link #scalars} is
     * unset or empty.
     */
    private static final List<String> RUNTIME_SCALARS = List.of(
        "Φ.number", "Φ.bytes", "Φ.string", "Φ.bool", "Φ.true", "Φ.false",
        "Φ.i8", "Φ.i16", "Φ.i32", "Φ.i64", "Φ.u8", "Φ.u16", "Φ.u32", "Φ.u64",
        "Φ.dataized"
    );

    /**
     * The directory where the prepared XMIR is saved, falling back to a
     * directory {@code Seqdir} numbers "pre-inference" when unset.
     */
    @Parameter(alias = "infer.prepared", property = "eo.infer.prepared")
    private File prepared;

    /**
     * The directory where {@code facts.xml} and {@code answers.xml} are
     * saved, falling back to a directory {@code Seqdir} numbers "inference"
     * when unset.
     */
    @Parameter(alias = "infer.tables", property = "eo.infer.tables")
    private File tables;

    /**
     * The kinds that are one shared copy each, born {@code canon}: the first
     * holds the data, and every other one holds the first. Each must have
     * one void, {@code @}. Unless set, or empty, these are the canon
     * of eo-runtime.
     */
    @Parameter(alias = "infer.canon", property = "eo.infer.canon")
    private List<String> canon;

    /**
     * The packages of list code: a copy that other code makes of a
     * formation under one of them starts a tag, which keeps apart what list
     * code does for different places of user code. Unless set, or
     * empty, these are the packages of list code of eo-runtime.
     */
    @Parameter(alias = "infer.roots", property = "eo.infer.roots")
    private List<String> roots;

    /**
     * The packages whose formations give back new data, so that a copy of
     * one of them takes no tag. Unless set, or empty, these are the
     * scalar packages of eo-runtime.
     */
    @Parameter(alias = "infer.scalars", property = "eo.infer.scalars")
    private List<String> scalars;

    /**
     * Ctor.
     */
    public MjInfer() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        final Path parsed = new Seqdir(this.target.toPath(), 2).dirs().once("parse");
        final Path prepped;
        if (this.prepared == null) {
            prepped = new Seqdir(this.target.toPath(), 2).dirs().once("pre-inference");
        } else {
            prepped = this.prepared.toPath();
        }
        final Path inference;
        if (this.tables == null) {
            inference = new Seqdir(this.target.toPath(), 2).dirs().once("inference");
        } else {
            inference = this.tables.toPath();
        }
        try (TjsForeign tojos = this.tojos()) {
            new Inferring(
                new Mapped<>(
                    tojo -> new Place(tojo.identifier()).make(parsed, MjAssemble.XMIR),
                    tojos.withXmir()
                ),
                parsed,
                prepped,
                inference,
                new Names(
                    MjInfer.orElse(this.canon, MjInfer.RUNTIME_CANON),
                    MjInfer.orElse(this.roots, MjInfer.RUNTIME_ROOTS),
                    MjInfer.orElse(this.scalars, MjInfer.RUNTIME_SCALARS)
                )
            ).exec();
        }
    }

    /**
     * The list, or the fallback when the list is unset or empty.
     *
     * @param list The list, maybe {@code null}
     * @param fallback The names of eo-runtime
     * @return The list or the fallback
     */
    private static List<String> orElse(final List<String> list, final List<String> fallback) {
        final List<String> names;
        if (list == null || list.isEmpty()) {
            names = fallback;
        } else {
            names = list;
        }
        return names;
    }
}
