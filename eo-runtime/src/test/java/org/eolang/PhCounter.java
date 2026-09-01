/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * An object that counts how many times its φ was calculated.
 *
 * <p>The count is exposed as an attribute of its own, so a test can read
 * it after a few dataizations and see whether the φ was reached more than
 * once.</p>
 *
 * @since 0.1.0
 */
final class PhCounter extends PhDefault {

    /**
     * Count.
     */
    private long count;

    /**
     * Make one, with all its attributes in place.
     *
     * <p>The attributes are attached here, and not in a constructor,
     * because both of them are expressions over the object itself, which
     * does not exist yet while its constructor runs.</p>
     *
     * @return The object
     */
    static Phi made() {
        final PhCounter made = new PhCounter();
        made.add(
            Phi.PHI,
            new AtOnce(
                new AtComposite(
                    made,
                    rho -> {
                        ++made.count;
                        return new Data.ToPhi(new byte[]{(byte) 0x01});
                    }
                )
            )
        );
        made.add("count", new AtComposite(made, rho -> new Data.ToPhi(made.count)));
        return made;
    }
}
