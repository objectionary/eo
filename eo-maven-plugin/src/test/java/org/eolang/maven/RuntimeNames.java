/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.List;
import java.util.function.Supplier;
import org.eolang.inference.Names;

/**
 * The names of eo-runtime the engine of {@link Inferring} has to know.
 *
 * <p>The goal takes them from the {@code pom.xml} of eo-runtime and has no
 * defaults, so a test that runs the engine on its own gives them here, the
 * one place the tests of this module write them, besides the configuration
 * {@link FakeMaven} plays the {@code pom.xml} with.</p>
 *
 * @since 0.76.0
 */
final class RuntimeNames implements Supplier<Names> {

    @Override
    public Names get() {
        return new Names(
            List.of("Φ.bytes", "Φ.number", "Φ.string"),
            List.of("Φ.tuple", "Φ.map", "Φ.set"),
            List.of(
                "Φ.number", "Φ.bytes", "Φ.string", "Φ.bool", "Φ.true", "Φ.false",
                "Φ.i8", "Φ.i16", "Φ.i32", "Φ.i64", "Φ.u8", "Φ.u16", "Φ.u32", "Φ.u64",
                "Φ.dataized"
            )
        );
    }
}
