/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * An object with a void attribute, a rho, and a φ that reads that void.
 *
 * <p>It stands for an ordinary formation of the runtime, the one most of
 * the {@link PhDefault} behaviour is watched on: a copy of it has its own
 * rho, its φ is memoized, and one of its attributes is an application of
 * another object, so that a contexted child with a rho is around.</p>
 *
 * @since 0.36.0
 */
final class PhInt extends PhDefault {

    /**
     * Make one, with all its attributes in place.
     *
     * <p>The attributes are attached here, and not in a constructor,
     * because two of them are expressions over the object itself, which
     * does not exist yet while its constructor runs.</p>
     *
     * @return The object
     */
    static Phi made() {
        final PhInt made = new PhInt();
        made.add(Phi.RHO, new AtRho());
        made.add("void", new AtVoid("void"));
        made.add("plus", new AtComposite(made, rho -> PhInt.formation()));
        made.add(
            Phi.PHI,
            new AtOnce(
                new AtComposite(
                    made,
                    rho -> rho.take("void")
                )
            )
        );
        made.add(
            "context-hasContextedChildWithSetRhoWhenFormed",
            new AtOnce(
                new AtComposite(
                    made,
                    rho -> {
                        final Phi plus = new Data.ToPhi(5L).take(
                            "plus"
                        ).copy();
                        plus.put(0, new Data.ToPhi(6L));
                        return plus;
                    }
                )
            )
        );
        return made;
    }

    /**
     * Make an empty formation with a rho of its own.
     * @return The formation
     */
    static Phi formation() {
        return new PhDefault(new Attrs(new Attr(Phi.RHO, new AtRho())));
    }
}
