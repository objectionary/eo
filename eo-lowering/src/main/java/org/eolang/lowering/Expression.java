/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * One XMIR fragment as a φ-calculus expression.
 *
 * <p>The fragment is rendered as a formation binding it to {@code φ}, a
 * complete expression of its own. It is not evaluatable alone, though:
 * the {@code Φ.number} and {@code Φ.bytes} references inside it resolve
 * only when {@code phino merge} joins this expression with the
 * {@link Universe}, whose root formation holds those method tables.</p>
 *
 * <p>The rendering is a serialization of the XMIR subtree and nothing
 * more: dispatches become dotted applications, literal carriers become
 * applications of {@code Φ.number}, {@code Φ.string} or {@code Φ.bytes},
 * and the bare hex datum becomes a {@code Δ} formation. No phi syntax is
 * ever parsed here, and a subtree holding anything else — a void, a
 * {@code ξ} reference, a formation — is refused, since its meaning
 * depends on a context this document does not carry.</p>
 *
 * @since 0.76.0
 */
public final class Expression {

    /**
     * The XMIR fragment to render, an {@code <o/>} element.
     */
    private final Xnav fragment;

    /**
     * Ctor.
     * @param xmir The XMIR fragment to render, an {@code <o/>} element
     */
    public Expression(final Xnav xmir) {
        this.fragment = xmir;
    }

    /**
     * The expression, in phi syntax.
     * @return The text for {@link Phino#dataize(String...)}
     */
    public String text() {
        return String.format(
            "⟦%n  φ ↦ %s%n⟧%n",
            Expression.rendered(this.fragment)
        );
    }

    private static String rendered(final Xnav node) {
        final String base = node.attribute("base").text().orElse("");
        final String out;
        if (base.isEmpty()) {
            out = String.format("⟦ Δ ⤍ %s ⟧", Expression.datum(node));
        } else if (base.charAt(0) == '.') {
            out = Expression.dispatched(node, base);
        } else if (base.startsWith("Φ.")) {
            out = Expression.applied(base, Expression.kids(node));
        } else {
            throw new IllegalStateException(
                String.format(
                    "The base '%s' depends on a context this document does not carry",
                    base
                )
            );
        }
        return out;
    }

    private static String dispatched(final Xnav node, final String base) {
        final List<Xnav> kids = Expression.kids(node);
        if (kids.isEmpty()) {
            throw new IllegalStateException(
                String.format("The dispatch '%s' has no receiver", base)
            );
        }
        return String.format(
            "%s%s%s",
            Expression.rendered(kids.get(0)),
            base,
            Expression.arguments(kids.subList(1, kids.size()))
        );
    }

    private static String applied(final String base, final Collection<Xnav> kids) {
        final String out;
        if (kids.isEmpty()) {
            out = base;
        } else {
            out = String.format("%s%s", base, Expression.arguments(kids));
        }
        return out;
    }

    private static String arguments(final Collection<Xnav> kids) {
        final String out;
        if (kids.isEmpty()) {
            out = "";
        } else {
            final Collection<String> parts = new ArrayList<>(kids.size());
            for (final Xnav kid : kids) {
                final String name = kid.attribute("as").text().orElse("");
                if (name.isEmpty()) {
                    throw new IllegalStateException(
                        "An argument without a binding name cannot be rendered"
                    );
                }
                parts.add(
                    String.format("%s ↦ %s", name, Expression.rendered(kid))
                );
            }
            out = String.format("(%s)", String.join(", ", parts));
        }
        return out;
    }

    private static String datum(final Xnav node) {
        return node.text().orElse("").replaceAll("\\s+", "");
    }

    private static List<Xnav> kids(final Xnav node) {
        return node.elements(Filter.withName("o")).collect(Collectors.toList());
    }
}
