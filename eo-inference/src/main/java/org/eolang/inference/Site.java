/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;

/**
 * One dispatch of a program, as the XMIR holds it.
 *
 * <p>Three things are ever asked of a dispatch: where it is written, which
 * name it takes, and which object it takes that name from. {@link Settled}
 * asks them of every dispatch of the program again on every turn it makes, so
 * they are read off the node and never searched for in the document the node
 * belongs to — a search knows nothing of where it was last time and looks
 * through the whole file again to find one attribute of one object.</p>
 *
 * <p>The object the name is taken from is the child that is not an argument:
 * {@code x.plus 5} takes {@code plus} from {@code x}, never from the
 * {@code 5}.</p>
 *
 * @since 0.71.0
 */
final class Site {

    /**
     * The dispatch.
     */
    private final Xnav dispatch;

    /**
     * Ctor.
     * @param made The dispatch, as the XMIR holds it
     */
    Site(final Xnav made) {
        this.dispatch = made;
    }

    /**
     * Where this dispatch is written.
     * @return The locator of it
     */
    String made() {
        return new Noted(this.dispatch).says("loc");
    }

    /**
     * The object this dispatch takes its name from.
     * @return The locator of it
     */
    String bearer() {
        return this.dispatch
            .elements(Filter.all(Filter.withName("o"), Filter.not(Filter.hasAttribute("as"))))
            .findFirst()
            .map(kid -> new Noted(kid).says("loc"))
            .orElse("");
    }

    /**
     * The name this dispatch takes.
     * @return The name, without the dot that says it is a dispatch
     */
    String name() {
        return new Noted(this.dispatch).says("base").substring(1);
    }
}
