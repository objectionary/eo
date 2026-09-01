/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * A node of the intermediate line tree, able to print itself.
 *
 * <p>It mirrors one {@code <line>} element of the tree produced by
 * {@code to-eo-tree.xsl}: the rendered head of an object, its optional
 * name suffix, a few flags telling it how to lay itself out (formation,
 * test attribute, reversed dispatch, data literal) and the children
 * (arguments or bindings).</p>
 *
 * <p>Printing is the node's own business. For every object it considers a
 * few renderings — a {@link Vertical} one, where the children go on their
 * own indented lines, a {@link Horizontal} one, where they are inlined, a
 * {@link Phi} one for a formation bound to nothing but its decoratee, and
 * a {@link Starred} one for a tuple applied at the tail — and keeps the
 * one with the smaller {@link Penalty}. The decision is made recursively,
 * bottom-up, against a {@link Style} that carries the indentation width
 * and the penalty weights, so the node never has to refer back to the
 * printer that started it.</p>
 *
 * @since 0.57.0
 */
final class Node {

    /**
     * The rendered head of the object (base, method, formation
     * params, data literal or {@code *}).
     */
    private final String base;

    /**
     * The rendered suffix ({@code > name}, {@code >>}, {@code !},
     * {@code /atom} or {@code :label}), possibly empty.
     */
    private final String tail;

    /**
     * Whether this object is a formation (its children are
     * bindings, so it is laid out vertically, unless its only
     * binding is the {@code φ} decoratee and the compact inline-phi
     * form fits on one line).
     */
    private final boolean abstractt;

    /**
     * Whether this object is a test attribute ({@code +> name}),
     * which R-6.5.3 requires to be preceded by a blank line.
     */
    private final boolean test;

    /**
     * Whether this object is a reversed dispatch ({@code method.});
     * a receiver-only one cannot be inlined as an argument.
     */
    private final boolean reversed;

    /**
     * Whether this object is a data literal (number, string, bytes),
     * so it may sit as a receiver in the suffix form of a reversed
     * dispatch ({@code 5.plus} instead of {@code plus. 5}).
     */
    private final boolean data;

    /**
     * The children (arguments or bindings), in order.
     */
    private final List<Node> children;

    /**
     * Ctor, from a {@code <line>} element.
     * @param line The {@code <line>} element
     */
    Node(final Xnav line) {
        this(
            line.attribute("base").text().orElse(""),
            line.attribute("tail").text().orElse(""),
            "yes".equals(line.attribute("abstract").text().orElse("no")),
            "yes".equals(line.attribute("test").text().orElse("no")),
            "yes".equals(line.attribute("reversed").text().orElse("no")),
            "yes".equals(line.attribute("data").text().orElse("no")),
            line.elements(Filter.withName("line"))
                .map(Node::new)
                .collect(Collectors.toList())
        );
    }

    /**
     * Ctor.
     * @param head The rendered head
     * @param suffix The rendered suffix
     * @param formation Whether it is a formation
     * @param attr Whether it is a test attribute
     * @param rev Whether it is a reversed dispatch
     * @param literal Whether it is a data literal
     * @param kids The children
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
     * Print this node as a (possibly multi-line) block at the given
     * indentation, picking the rendering with the lowest penalty.
     *
     * <p>When the node is a reversed dispatch whose receiver has a one-line
     * inline form ({@code plus. 5 3}, {@code div. (a.plus b) c},
     * {@code ^. read. (input.read 4096)}), its equivalent suffix shape
     * ({@code 5.plus 3}, {@code (a.plus b).div c},
     * {@code (input.read 4096).read.^}) is laid out too and the lower-penalty
     * one is kept — the same penalty comparison that already decides inline
     * versus vertical. A lone-data or plain-chain receiver glues bare and its
     * suffix can only tie or win, so a tie (both fit the width) resolves to
     * the suffix, the shorter form; a compound receiver pays a {@code BRACKET}
     * for its parentheses, so its suffix is kept only when the saved
     * indentation and repeated base characters outweigh that one bracket
     * (#5650).</p>
     *
     * <p>When the dispatch carries an inline binding ({@code :hey}), the
     * suffix shape is taken whatever the penalties say. The vertical head of
     * a reversed dispatch ends with the dot, and a binding glued onto it
     * ({@code print.:hey}) is text the grammar does not accept, so the
     * printer would write a file the next build cannot read (#7709). The
     * suffix shape puts the receiver in front of the dot and leaves the
     * binding after a name, where it parses.</p>
     *
     * @param style The style to lay out in
     * @param indent The indentation level
     * @return The rendered block
     */
    String print(final Style style, final int indent) {
        String best = this.shaped(style, indent);
        final Optional<Node> suffix = this.suffixed();
        if (suffix.isPresent()) {
            final String alt = suffix.get().shaped(style, indent);
            if (this.labelled() || style.points(alt) <= style.points(best)) {
                best = alt;
            }
        }
        return best;
    }

    /**
     * Print this node on the lines below the head of its parent: the block
     * itself, preceded by the newline that opens it and, for a test
     * attribute, by the blank line R-6.5.3 requires in front of it.
     * @param style The style to lay out in
     * @param indent The indentation level
     * @return The rendered block with its leading newlines
     */
    String indented(final Style style, final int indent) {
        return this.opened()
            .append(this.lined().print(style, indent))
            .toString();
    }

    /**
     * Print this node on the lines below the head of its parent, keeping
     * its own children beneath it whatever the penalties say.
     *
     * <p>A method continuation ({@code .y}, §3.5) parses only under a
     * vertical application: it attaches to the lines above it and the
     * horizontal form has no place for it. So an application a
     * continuation hangs on stays vertical however cheap its one-line
     * spelling looks, or the printer writes a file the next build cannot
     * read (#8058).</p>
     *
     * @param style The style to lay out in
     * @param indent The indentation level
     * @return The rendered block with its leading newlines
     */
    String stacked(final Style style, final int indent) {
        return this.opened()
            .append(this.lined().vertical(style, indent))
            .toString();
    }

    /**
     * Whether this node is a nameless method-dispatch continuation
     * ({@code .y}, {@code ?.y}), which dispatches on the lines above it
     * instead of carrying a receiver of its own.
     * @return True when this node continues the sibling above it
     */
    boolean continuation() {
        return this.children.isEmpty() && this.tail.isEmpty()
            && (this.base.startsWith(".") || this.base.startsWith("?."));
    }

    /**
     * Print this node with its children laid out beneath its head.
     * @param style The style to lay out in
     * @param indent The indentation level
     * @return The rendered block
     */
    String vertical(final Style style, final int indent) {
        return new Vertical(this.base.concat(this.tail), this.children)
            .print(style, indent);
    }

    /**
     * Spell this node as it appears in an argument slot: bracketed when
     * it applies arguments of its own, bare when it is a single token,
     * with the {@code !} of an anonymous inline const appended last.
     *
     * <p>The brackets are decided on the argument's effective
     * (suffix-resolved) shape, not its raw one. A data-receiver dispatch
     * such as {@code 01-.as-bool} is stored as a reversed head over a data
     * child, so its raw node has a child (the receiver) yet it takes no
     * arguments and is a single token — {@link #suffixed()} folds the
     * receiver back into the base, leaving no children, so it is spelled
     * bare. Wrapping it as {@code (01-.as-bool)} would produce EO that
     * fails to parse with "redundant parentheses around a single token"
     * (#5591).</p>
     *
     * <p>An anonymous inline const argument (#5821) is spelled bare, through
     * {@link #bare()}, and its {@code !} marker appended afterwards — after
     * the closing bracket when the argument is one. Inside the brackets the
     * marker binds to the last argument instead: {@code (inc m!)} reads as
     * {@code inc (m!)}, shrinking the const from the whole application down to
     * one of its arguments and silently changing the program (#5902).</p>
     *
     * @return The spelling, or empty if this node cannot be inlined
     */
    Optional<String> inlined() {
        final Optional<String> result;
        if (this.constant()) {
            result = this.bare().braced().map(text -> text.concat("!"));
        } else {
            result = this.braced();
        }
        return result;
    }

    /**
     * Spell this node inline, as it would appear as an argument (without
     * its own name suffix), or empty if it can't be inlined safely. A
     * data-receiver dispatch is spelled in its suffix shape ({@code
     * 5.plus 3}), never the reversed one.
     *
     * <p>Any suffix in the tail ({@code > name}, {@code >>}, {@code !}) has no
     * inline spelling and blocks inlining. An anonymous inline const argument
     * (#5821) is spelled by {@link #inlined()}, which strips the {@code !}
     * here and appends it where it belongs (#5902).</p>
     *
     * @return The inlined content, or empty
     */
    Optional<String> flat() {
        return this.suffixed().orElse(this).spelled();
    }

    /**
     * The same node with its suffix dropped.
     *
     * <p>An inline const argument that applies arguments of its own is
     * parenthesised, and its {@code !} marker has to sit outside the
     * brackets ({@code (inc m)!}), so {@link #inlined()} spells the node
     * without the suffix and appends the marker itself (#5902).</p>
     *
     * @return The node without its suffix
     */
    Node bare() {
        return new Node(
            this.base, "", this.abstractt, this.test,
            this.reversed, this.data, this.children
        );
    }

    /**
     * Build the body of the hybrid inline-phi form for this decoratee: its
     * head kept in front of {@code marker}, its arguments as children.
     *
     * <p>When this decoratee applies a trailing tuple ({@code seq *},
     * {@code sprintf *1}), the {@code *N} marker is glued onto that head
     * line ({@code seq * > [m]}) through {@link #glued} and every argument
     * becomes a child, mirroring the {@link Starred} idiom and saving a
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
     * {@link #suffixed()} rebuilds it as a non-reversed, single-child
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
     * Whether this node applies arguments of its own, so it has a hybrid
     * inline-phi form: not a formation, whose children are bindings rather
     * than arguments, and not a bare token, which has nothing to lay out
     * beneath a marker.
     * @return True when this node applies arguments
     */
    boolean applied() {
        return !this.abstractt && !this.children.isEmpty();
    }

    /**
     * Whether no line in this node's children carries a name suffix, so
     * their subtrees are safe to fold into a compact only-phi formation.
     * @return True when every child subtree is nameless
     * @see #nameless()
     */
    boolean anonymous() {
        return this.children.stream().allMatch(Node::nameless);
    }

    private StringBuilder opened() {
        final StringBuilder block = new StringBuilder();
        if (this.test) {
            block.append('\n');
        }
        return block.append('\n');
    }

    private String shaped(final Style style, final int indent) {
        final Optional<String> star = new Starred(this).print(style, indent);
        final String result;
        if (star.isPresent() && this.children.size() > 1) {
            result = star.get();
        } else {
            String best = this.vertical(style, indent);
            final Optional<String> flat = this.horizontal(style, indent);
            if (flat.isPresent()
                && (this.forced() || this.labelled()
                || style.points(flat.get()) <= style.points(best))) {
                best = flat.get();
            }
            if (star.isPresent() && style.points(star.get()) < style.points(best)) {
                best = star.get();
            }
            result = best;
        }
        return result;
    }

    private Optional<String> horizontal(final Style style, final int indent) {
        final Optional<String> result;
        if (this.abstractt) {
            result = this.phi(style, indent);
        } else if (this.children.isEmpty()) {
            result = Optional.empty();
        } else {
            result = new Horizontal(
                this.base, this.tail, new Arguments(this.children)
            ).print(style, indent);
        }
        return result;
    }

    private Optional<String> phi(final Style style, final int indent) {
        final Optional<String> result;
        if (this.children.size() == 1
            && " > @".equals(this.children.get(0).tail)) {
            result = new Phi(
                this.base, this.tail, this.children.get(0)
            ).print(style, indent);
        } else {
            result = Optional.empty();
        }
        return result;
    }

    private Optional<Node> suffixed() {
        Optional<Node> result = Optional.empty();
        if (this.reversed && !this.children.isEmpty()) {
            final String dot;
            if (this.base.endsWith("?.")) {
                dot = "?.";
            } else {
                dot = ".";
            }
            final String head = this.base.substring(0, this.base.length() - dot.length());
            if (!"$".equals(head)) {
                result = this.children.get(0).braced().map(
                    glued -> new Node(
                        String.join(dot, glued, head),
                        this.tail, this.abstractt, this.test, false, false,
                        this.children.subList(1, this.children.size())
                    )
                );
            }
        }
        return result;
    }

    private Optional<String> braced() {
        final Node node = this.suffixed().orElse(this);
        return node.spelled().map(node::wrapped);
    }

    private String wrapped(final String text) {
        final String result;
        if (this.children.isEmpty()) {
            result = text;
        } else {
            result = "(".concat(text).concat(")");
        }
        return result;
    }

    private Optional<String> spelled() {
        final Optional<String> result;
        if (this.reversed && this.children.size() <= 1) {
            result = Optional.empty();
        } else if (this.abstractt || !this.tail.isEmpty() || "*".equals(this.base)) {
            result = Optional.empty();
        } else if (this.children.isEmpty()) {
            result = Optional.of(this.base);
        } else {
            result = new Arguments(this.children).joined()
                .map(args -> String.join(" ", this.base, args));
        }
        return result;
    }

    private Node lined() {
        final Node result;
        if (this.constant()) {
            result = new Node(
                this.base, " >>!", this.abstractt, this.test,
                this.reversed, this.data, this.children
            );
        } else {
            result = this;
        }
        return result;
    }

    private boolean nameless() {
        return (this.tail.isEmpty() || " > @".equals(this.tail))
            && this.anonymous();
    }

    private boolean marked() {
        return !this.abstractt && !this.reversed && !"*".equals(this.base);
    }

    private boolean absorbed(final int size) {
        return size > 1 || this.base.indexOf('.') < 0;
    }

    private boolean stars() {
        return "*".equals(this.base) && !this.abstractt
            && !this.children.isEmpty() && this.tail.isEmpty();
    }

    private boolean constant() {
        return "!".equals(this.tail);
    }

    private boolean labelled() {
        return this.reversed && this.tail.startsWith(":");
    }

    private boolean forced() {
        return "|".equals(this.base) && this.tail.isEmpty()
            || this.children.stream().anyMatch(Node::constant);
    }
}
