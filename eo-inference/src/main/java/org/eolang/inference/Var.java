/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * An object whose type only a caller can say.
 *
 * <p>A void is the one thing in a program that is deliberately left open, and
 * what it holds is decided by whoever fills it. So the row says that and
 * nothing more, and says it under the locator of the void, which is the name
 * every mention of that variable is written with: {@code Φ.inc.x.next} is the
 * {@code next} of whatever fills {@code x}, true of every caller and concrete
 * for none.</p>
 *
 * <p>Nothing is carried here, not even the name, since the row is keyed by the
 * locator and the locator is the name.</p>
 *
 * @since 0.69.0
 */
final class Var implements Type {

    @Override
    public Directives directives() {
        return new Directives().add("var").up();
    }
}
