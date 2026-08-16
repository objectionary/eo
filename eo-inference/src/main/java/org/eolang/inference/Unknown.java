/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * A dispatch that nothing could answer.
 *
 * <p>A dispatch is only known to be unanswerable once the passes have
 * stopped adding pairs: the passes keep going while they add, so a row
 * written before they settled would call something unanswerable that a
 * later pass answers. An unknown is a fact like any other — the silence
 * around an object that was never looked at says something else than the
 * silence around one that was looked at and gave nothing.</p>
 *
 * @since 0.69.0
 */
final class Unknown implements Type {

    @Override
    public Directives directives() {
        return new Directives().add("unknown").up();
    }
}
