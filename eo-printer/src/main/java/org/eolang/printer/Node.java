/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.List;
import java.util.stream.Collectors;

/**
 * A node of the intermediate line tree consumed by {@link Pretty}.
 *
 * <p>It mirrors one {@code <line>} element of the tree produced by
 * {@code to-eo-tree.xsl}: the rendered head of an object, its optional
 * name suffix, a few flags telling {@link Pretty} how to lay it out
 * (formation, test attribute, reversed dispatch, data literal) and the
 * children (arguments or bindings). The dependency runs one way only,
 * {@link Pretty} reads a {@code Node}; a {@code Node} never refers back to
 * {@link Pretty}. Its fields and the constructor are package-private so
 * {@link Pretty} and its static helpers keep their direct field access,
 * the same exposure a nested class would grant.</p>
 *
 * @since 0.57.0
 */
final class Node {

    /**
     * The rendered head of the object (base, method, formation
     * params, data literal or {@code *}).
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    final String base;

    /**
     * The rendered suffix ({@code > name}, {@code >>}, {@code !},
     * {@code /atom} or {@code :label}), possibly empty.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    final String tail;

    /**
     * Whether this object is a formation (its children are
     * bindings, so it is laid out vertically, unless its only
     * binding is the {@code φ} decoratee and the compact inline-phi
     * form fits on one line).
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    final boolean abstractt;

    /**
     * Whether this object is a test attribute ({@code +> name}),
     * which R-6.5.3 requires to be preceded by a blank line.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    final boolean test;

    /**
     * Whether this object is a reversed dispatch ({@code method.});
     * a receiver-only one cannot be inlined as an argument.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    final boolean reversed;

    /**
     * Whether this object is a data literal (number, string, bytes),
     * so it may sit as a receiver in the suffix form of a reversed
     * dispatch ({@code 5.plus} instead of {@code plus. 5}).
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    final boolean data;

    /**
     * The children (arguments or bindings), in order.
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    final List<Node> children;

    /**
     * Ctor.
     * @param head The rendered head
     * @param suffix The rendered suffix
     * @param formation Whether it is a formation
     * @param attr Whether it is a test attribute
     * @param rev Whether it is a reversed dispatch
     * @param literal Whether it is a data literal
     * @param kids The children
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    Node(final String head, final String suffix, final boolean formation,
        final boolean attr, final boolean rev, final boolean literal,
        final List<Node> kids) {
        this.base = head;
        this.tail = suffix;
        this.abstractt = formation;
        this.test = attr;
        this.reversed = rev;
        this.data = literal;
        this.children = kids;
    }

    /**
     * Build a node from a {@code <line>} element.
     * @param line The {@code <line>} element
     * @return The node
     */
    static Node parse(final Xnav line) {
        return new Node(
            line.attribute("base").text().orElse(""),
            line.attribute("tail").text().orElse(""),
            "yes".equals(line.attribute("abstract").text().orElse("no")),
            "yes".equals(line.attribute("test").text().orElse("no")),
            "yes".equals(line.attribute("reversed").text().orElse("no")),
            "yes".equals(line.attribute("data").text().orElse("no")),
            line.elements(Filter.withName("line"))
                .map(Node::parse)
                .collect(Collectors.toList())
        );
    }

    /**
     * Whether this node carries no name suffix anywhere in its subtree.
     *
     * <p>A line is "named" when its {@code tail} holds a {@code > name},
     * {@code > [params]} or {@code >>} suffix. This walks the node and all
     * its descendants, so a named line nested below the top level — inside
     * a tuple, a dispatch, or an application — is caught too. It decides
     * whether a decoratee's whole subtree is safe to fold into a compact
     * only-phi formation, which binds nothing but its {@code φ} decoratee
     * (issue #5604).</p>
     *
     * @return True when neither this node nor any descendant is named
     */
    boolean nameless() {
        return this.tail.isEmpty()
            && this.children.stream().allMatch(Node::nameless);
    }

    /**
     * Whether this node is a plain application whose last child is a
     * tuple that can be glued to its head as a trailing {@code *}.
     *
     * <p>A formation lays its children out as bindings and a reversed
     * dispatch keeps its receiver first, so neither is a plain
     * application and neither qualifies. The star must be the head's
     * only child: a bare trailing {@code *} absorbs the indented
     * siblings beneath it into the tuple, so this round-trips only for
     * the genuine {@code seq *} idiom. When a preceding argument shares
     * the line ({@code sm.win32 "getenv" *}), the parser reads it as a
     * complete application with an empty tuple and rejects the indented
     * child, so the hybrid must not fire (issue #5622). The single child
     * must itself be a gluable star (see {@link #stars()}).</p>
     *
     * <p>The head must also be a plain base, not a dotted method dispatch
     * ({@code "literal".printf}, {@code 5.plus}). A bare trailing
     * {@code *} is absorbed by the parser only after a plain leading
     * application ({@code seq *}, {@code map *}, {@code switch *}); after
     * a method dispatch it reads as a complete application with an empty
     * tuple and rejects the indented elements. A data-receiver dispatch
     * is stored reversed and so already fails the {@code !reversed} guard,
     * but {@link Pretty#suffixed} rebuilds it as a non-reversed, single-child
     * node whose base is exactly such a dispatch, and that alternative is
     * weighed for the hybrid too — barring a dotted base here keeps it,
     * and any genuine dotted dispatch, on the ordinary {@code * elem}
     * child that round-trips (issue #5624).</p>
     *
     * @return True when the trailing-star hybrid form is applicable
     */
    boolean tuply() {
        return this.gluer()
            && this.children.size() == 1
            && this.children.get(0).stars();
    }

    /**
     * Whether this node is a plain application head onto which a
     * trailing {@code *} may be glued: not a formation, not a reversed
     * dispatch, and not a dotted method dispatch. See {@link #tuply()}
     * for why a dotted base ({@code "literal".printf}, {@code 5.plus})
     * is excluded (issue #5624).
     * @return True when the head can carry a glued trailing star
     */
    boolean gluer() {
        return !this.abstractt && !this.reversed && this.base.indexOf('.') < 0;
    }

    /**
     * Whether this node is a non-empty, unnamed {@code *} tuple — one
     * that may be glued to the tail of an applying object's line.
     * @return True when this node is a gluable star
     */
    boolean stars() {
        return "*".equals(this.base) && !this.abstractt
            && !this.children.isEmpty() && this.tail.isEmpty();
    }

    /**
     * Build the synthetic node that renders this tuple glued to the
     * tail of {@code applier}'s line: {@code head *} on one line (with
     * the applier's own name suffix), the tuple's elements as children.
     *
     * <p>The star is the applier's only child (see {@link #tuply()}), so
     * nothing precedes it on the line — the head is the applier's bare
     * base glued straight to the {@code *}.</p>
     *
     * @param applier The object applying this tuple
     * @return The glued node, laid out vertically by the caller
     */
    Node glued(final Node applier) {
        return new Node(
            String.join(" ", applier.base, this.base), applier.tail,
            false, false, false, false, this.children
        );
    }
}
