/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * An integer-like object for the tests.
 *
 * @since 0.36.0
 */
final class Integral extends PhDefault {

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
        final Integral made = new Integral();
        made.add(Phi.RHO, new AtRho());
        made.add("void", new AtVoid("void"));
        made.add("plus", new AtComposite(made, rho -> Integral.formation()));
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

    static Phi formation() {
        return new PhDefault(new Attrs(new Attr(Phi.RHO, new AtRho())));
    }
}
