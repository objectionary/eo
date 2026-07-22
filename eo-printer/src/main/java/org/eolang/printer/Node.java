/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
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
     * {@code > [params] > name} or {@code >>} suffix — a named or auto-named
     * attribute. This walks the node and all its descendants, so a named line
     * nested below the top level — inside a tuple, a dispatch, or an
     * application — is caught too. It decides whether a decoratee's whole
     * subtree is safe to inline as the {@code φ} of a compact only-phi
     * formation: the decoratee's arguments must stay unnamed, since a named
     * argument would, once the formation is collapsed, be misread as a
     * sibling attribute of the formation rather than an argument of the
     * decoratee (issue #5604). Named attributes of the formation itself are
     * legal (§4.5, #5754) and handled separately in {@link Pretty}, not
     * through this guard.</p>
     *
     * <p>A bare {@code > @} suffix is not a name: it marks the {@code φ}
     * decoratee, which is exactly what an only-phi formation binds, so a
     * nested anonymous object (an argument such as {@code m > [m]}, whose
     * subtree carries an inner {@code m > @}) is legal and must not withhold
     * the fold. Only a genuine named or auto-named attribute ({@code > name},
     * {@code >>}) does, since a named argument of the decoratee cannot be
     * inlined as part of the {@code φ}.</p>
     *
     * @return True when neither this node nor any descendant is named
     */
    boolean nameless() {
        return (this.tail.isEmpty() || " > @".equals(this.tail))
            && this.children.stream().allMatch(Node::nameless);
    }

    /**
     * Whether this node is a plain application whose last child is a
     * tuple that can be compacted onto its head as a trailing
     * {@code *N} marker.
     *
     * <p>A formation lays its children out as bindings and a reversed
     * dispatch keeps its receiver first, so neither is a plain
     * application and neither qualifies (a reversed compact-tuple head
     * such as {@code joined. *1} is not yet parseable). A bare tuple
     * head ({@code base == "*"}) is a tuple literal, not an object
     * applying a trailing tuple, so it is excluded too: its own elements
     * are already tuple elements and gluing a {@code *N} marker onto it
     * ({@code * *2}) would be a confusing self-application, never the
     * intended compaction. The last child must itself be a gluable star
     * (see {@link #stars()}); any leading children are the {@code N}
     * positional arguments the marker keeps in front of the tuple.</p>
     *
     * <p>When the star is the sole child ({@code N == 0}) the head must
     * be a plain base, not a dotted method dispatch
     * ({@code "literal".printf}, {@code 5.plus}). The bare trailing
     * {@code *} is absorbed by the parser only after a plain leading
     * application ({@code seq *}, {@code map *}); after a method dispatch
     * it reads as a complete application with an empty tuple and rejects
     * the indented elements. A data-receiver dispatch is stored reversed
     * and so already fails the {@code !reversed} guard, but
     * {@link Pretty#suffixed} rebuilds it as a non-reversed, single-child
     * node whose base is exactly such a dispatch — barring a dotted base
     * for {@code N == 0} keeps it, and any genuine dotted dispatch, on the
     * ordinary {@code * elem} child that round-trips (issues #5622,
     * #5624). With {@code N >= 1} the count sits on the head's line, so
     * the {@code *N} marker round-trips after a dotted dispatch too
     * ({@code string.sprintf *1}) and a dotted base is allowed
     * (issue #5648).</p>
     *
     * @return True when the trailing-star hybrid form is applicable
     */
    boolean tuply() {
        final int size = this.children.size();
        return size > 0
            && this.marked()
            && this.children.get(size - 1).stars()
            && this.absorbed(size);
    }

    /**
     * Whether this head can carry a trailing-star marker at all: not a
     * formation (its children are bindings), not a reversed dispatch (a
     * reversed compact-tuple head is not yet parseable), and not a bare
     * tuple literal ({@code base == "*"}), whose elements are already
     * tuple elements.
     * @return True when the head may take a {@code *N} marker
     */
    boolean marked() {
        return !this.abstractt && !this.reversed && !"*".equals(this.base);
    }

    /**
     * Whether the trailing tuple is absorbed correctly at the given
     * child count. With {@code N >= 1} leading arguments the {@code *N}
     * marker sits on the head's line, so a dotted method dispatch is
     * fine; with the bare {@code *} ({@code N == 0}) the head must be a
     * plain base, since after a method dispatch the parser reads a
     * complete application with an empty tuple (issues #5622, #5624).
     * @param size The number of children
     * @return True when the shape round-trips at this count
     */
    boolean absorbed(final int size) {
        return size > 1 || this.base.indexOf('.') < 0;
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
     * Build the synthetic node that renders this application with its
     * trailing tuple compacted to a {@code *N} marker: {@code head *N}
     * on one line (with the node's own name suffix), every argument as
     * an indented child.
     *
     * <p>The last child is the gluable star (see {@link #tuply()}); the
     * {@code N} children in front of it are the leading positional
     * arguments the marker keeps. The head line carries {@code head *N},
     * where {@code N} is that leading count, and the children are the
     * leading arguments followed by the tuple's own elements. When
     * {@code N == 0} the bare {@code *} is written (the {@code seq *}
     * idiom), matching the parser's default count of zero.</p>
     *
     * @return The glued node, laid out vertically by the caller
     */
    Node glued() {
        final int last = this.children.size() - 1;
        final List<Node> kids = new ArrayList<>(
            this.children.subList(0, last)
        );
        kids.addAll(this.children.get(last).children);
        final String marker;
        if (last == 0) {
            marker = "*";
        } else {
            marker = "*".concat(Integer.toString(last));
        }
        return new Node(
            String.join(" ", this.base, marker), this.tail,
            false, false, false, false, kids
        );
    }

    /**
     * Build the body of the hybrid inline-phi form for this decoratee: its
     * head kept in front of {@code marker}, its arguments as children.
     *
     * <p>When this decoratee applies a trailing tuple ({@code seq *},
     * {@code sprintf *1}), the {@code *N} marker is glued onto that head
     * line ({@code seq * > [m]}) through {@link #glued} and every argument
     * becomes a child, mirroring the {@code starred} idiom and saving a
     * line and an indent level; the parser absorbs a compact tuple in
     * inline-phi position, so this round-trips (issue #5626). Otherwise
     * the arguments stay as this node's children, laid out vertically by
     * the caller.</p>
     *
     * @param marker The inline-phi marker ({@code  > [params] > name})
     * @return The body node to lay out vertically
     */
    Node hybrid(final String marker) {
        final Node plain = new Node(
            this.base, marker, false, false,
            this.reversed, this.data, this.children
        );
        final Node body;
        if (this.tuply()) {
            body = plain.glued();
        } else {
            body = plain;
        }
        return body;
    }
}
