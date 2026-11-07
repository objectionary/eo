/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.List;

/**
 * A type as an annotation writes it into the XMIR, one string read as a
 * tree.
 *
 * <p>The parser keeps a whole type in one attribute: the return of an atom
 * in {@code @atom}, the own type of a void in {@code @type}, and each member
 * of a call back list in {@code @args}. A type is a type variable,
 * {@code A} to {@code F}, or a path with a bracket list right after it or
 * none. The members of the list are split by a comma outside any inner
 * bracket, and each of them is a type in its turn: the i-th one types the
 * i-th void of the object the path names, in the order the voids are
 * declared. So {@code Φ.posix.status-return(Φ.number,Φ.string)} is a
 * {@code status-return} with a number in its first void and a string in its
 * second. A {@code ?} that ends the own type of a void says the void may
 * hold a terminator, and is dropped here. The parser has already checked the
 * grammar, so nothing is checked again.</p>
 *
 * @since 0.76.0
 */
public final class Annotation {

    /**
     * The type, as the XMIR writes it.
     */
    private final String text;

    /**
     * Ctor.
     *
     * @param type The type, as the XMIR writes it
     */
    public Annotation(final String type) {
        this.text = type;
    }

    @Override
    public String toString() {
        return this.bare();
    }

    /**
     * The path or the type variable it starts with.
     *
     * @return The path or the variable
     */
    public String head() {
        final String bare = this.bare();
        final int open = bare.indexOf('(');
        final String found;
        if (open < 0) {
            found = bare;
        } else {
            found = bare.substring(0, open);
        }
        return found;
    }

    /**
     * The types its bracket list gives the voids of the object it names, in
     * the order written.
     *
     * @return The types, none when it has no brackets
     */
    public List<Annotation> members() {
        final String bare = this.bare();
        final int open = bare.indexOf('(');
        final List<Annotation> found;
        if (open < 0) {
            found = new ArrayList<>(0);
        } else {
            found = new Annotation(bare.substring(open + 1, bare.length() - 1)).split();
        }
        return found;
    }

    /**
     * Whether it is a type variable.
     *
     * @return TRUE if it is
     */
    boolean variable() {
        final String head = this.head();
        return head.length() == 1 && head.charAt(0) >= 'A' && head.charAt(0) <= 'F';
    }

    private String bare() {
        final String found;
        if (this.text.endsWith("?")) {
            found = this.text.substring(0, this.text.length() - 1);
        } else {
            found = this.text;
        }
        return found;
    }

    private List<Annotation> split() {
        final String type = this.bare();
        final List<Annotation> found = new ArrayList<>(2);
        int depth = 0;
        int start = 0;
        for (int pos = 0; pos < type.length(); ++pos) {
            final char glyph = type.charAt(pos);
            if (glyph == '(') {
                depth += 1;
            } else if (glyph == ')') {
                depth -= 1;
            } else if (glyph == ',' && depth == 0) {
                found.add(new Annotation(type.substring(start, pos)));
                start = pos + 1;
            }
        }
        found.add(new Annotation(type.substring(start)));
        return found;
    }
}
