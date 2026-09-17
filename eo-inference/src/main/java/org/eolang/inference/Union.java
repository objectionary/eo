/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import org.xembly.Directives;

/**
 * A choice between several types.
 *
 * <p>One object is one type, and a place where several of them turn up is not
 * one of them and not none: a void that two callers fill differently is a
 * choice between what they filled it with. The members are written inside it,
 * each in the form it goes by elsewhere, since a member of a choice is a type
 * like any other and may be a choice in its turn.</p>
 *
 * @since 0.69.0
 */
final class Union implements Type {

    /**
     * The types this one is a choice between.
     */
    private final Collection<Type> members;

    /**
     * Ctor.
     *
     * @param types The types this one is a choice between
     */
    Union(final Collection<Type> types) {
        this.members = types;
    }

    @Override
    public Directives directives() {
        final Directives dirs = new Directives().add("union");
        for (final Type member : this.members) {
            dirs.append(member.directives());
        }
        return dirs.up();
    }
}
