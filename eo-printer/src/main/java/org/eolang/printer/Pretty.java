/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Penalty-based pretty printer of EO code.
 *
 * <p>It takes the intermediate "line tree" produced by
 * {@code to-eo-tree.xsl} (an {@code <eo>} element with a {@code
 * <preamble>} and nested {@code <line>} elements) and lays it out as
 * EO source. For every object it considers two renderings — a
 * vertical one, where the arguments go on their own indented lines,
 * and a horizontal one, where they are inlined (wrapped in
 * parentheses when needed) — and keeps the one with the smaller
 * {@link Penalty}. The decision is made recursively, bottom-up.</p>
 *
 * @since 0.57.0
 */
final class Pretty {

    /**
     * The default weights: an empty map, so every key uses its fallback.
     */
    private static final Map<PenaltyKey, Integer> DEFAULTS = Collections.emptyMap();

    /**
     * The {@code <eo>} element produced by {@code to-eo-tree.xsl}.
     */
    private final Xnav root;

    /**
     * The overridden penalty weights, by key.
     */
    private final Map<PenaltyKey, Integer> weights;

    /**
     * Ctor, using the default penalty weights.
     * @param element The {@code <eo>} element
     */
    Pretty(final Xnav element) {
        this(element, Pretty.DEFAULTS);
    }

    /**
     * Ctor.
     * @param element The {@code <eo>} element
     * @param config The overridden weights; absent keys use their defaults
     */
    Pretty(final Xnav element, final Map<PenaltyKey, Integer> config) {
        this.root = element;
        this.weights = config;
    }

    /**
     * Render the whole program as pretty EO source.
     * @return EO source code
     */
    String asString() {
        final StringBuilder out = new StringBuilder(
            this.root.element("preamble").text().orElse("")
        );
        this.root.elements(Filter.withName("line"))
            .findFirst()
            .map(line -> this.layout(Pretty.Node.parse(line), 0))
            .ifPresent(out::append);
        return out.append('\n').toString();
    }

    /**
     * A single level of indentation, whose width is the {@code STEP} weight.
     * @return The indentation string
     */
    private String step() {
        return " ".repeat(
            this.weights.getOrDefault(PenaltyKey.STEP, PenaltyKey.STEP.fallback())
        );
    }

    /**
     * Lay out a node as a (possibly multi-line) block at the given
     * indentation, picking the rendering with the lowest penalty.
     *
     * <p>When the node is a reversed dispatch on a lone data receiver
     * ({@code plus. 5 3}), its equivalent suffix shape ({@code 5.plus 3})
     * is laid out too and the lower-penalty one is kept — the same
     * penalty comparison that already decides inline versus vertical. The
     * suffix shape is never longer and its vertical form has one fewer
     * child line, so it can only tie or win; a tie (both fit the width)
     * resolves to the suffix, the shorter and more readable form.</p>
     *
     * @param node The node
     * @param indent The indentation level
     * @return The rendered block
     */
    private String layout(final Pretty.Node node, final int indent) {
        String best = this.shaped(node, indent);
        final Optional<Pretty.Node> suffix = Pretty.suffixed(node);
        if (suffix.isPresent()) {
            final String alt = this.shaped(suffix.get(), indent);
            if (new Penalty(alt, this.weights).points()
                <= new Penalty(best, this.weights).points()) {
                best = alt;
            }
        }
        return best;
    }

    /**
     * Lay out a single shape of a node, picking the lowest-penalty of its
     * candidate renderings.
     *
     * <p>The vertical rendering is always available. The horizontal
     * (single-line) one is tried when the node can be inlined. For a
     * formation whose only binding is the {@code φ} decoratee, when no
     * single line exists (the decoratee's arguments must go vertical),
     * the {@link #hybrid} form — the compact inline-phi marker with the
     * arguments beneath — is offered instead, so the penalty search can
     * prefer it over the fully verbose vertical shape (issue #5594).</p>
     *
     * @param node The node
     * @param indent The indentation level
     * @return The rendered block
     */
    private String shaped(final Pretty.Node node, final int indent) {
        String best = this.vertical(node, indent);
        final Optional<String> flat = this.horizontal(node, indent);
        if (flat.isPresent()
            && new Penalty(flat.get(), this.weights).points()
            < new Penalty(best, this.weights).points()) {
            best = flat.get();
        } else if (flat.isEmpty()) {
            final Optional<String> hybrid = this.hybrid(node, indent);
            if (hybrid.isPresent()
                && new Penalty(hybrid.get(), this.weights).points()
                < new Penalty(best, this.weights).points()) {
                best = hybrid.get();
            }
        }
        return best;
    }

    /**
     * The suffix shape of a reversed dispatch on a lone data receiver, if
     * this node is one.
     *
     * <p>A data-receiver dispatch is stored as a reversed head
     * ({@code plus.}) whose first child is the data literal receiver, since
     * a literal cannot fold into a dotted {@code @base} the way a named
     * receiver does. This rebuilds it in suffix position — the receiver
     * glued to the method ({@code 5.plus}) with the remaining arguments
     * kept as children — so both shapes can be weighed against each other.
     * A compound receiver (one with children of its own) has no suffix
     * form and yields empty, leaving the reversed head untouched.</p>
     *
     * @param node The node
     * @return The suffix-shaped node, or empty if it doesn't apply
     */
    private static Optional<Pretty.Node> suffixed(final Pretty.Node node) {
        Optional<Pretty.Node> result = Optional.empty();
        if (node.reversed && !node.children.isEmpty()) {
            final Pretty.Node receiver = node.children.get(0);
            if (receiver.data && receiver.children.isEmpty()
                && receiver.tail.isEmpty()) {
                result = Optional.of(
                    new Pretty.Node(
                        String.join(
                            ".", receiver.base,
                            node.base.substring(0, node.base.length() - 1)
                        ),
                        node.tail, node.abstractt, node.test, false, false,
                        node.children.subList(1, node.children.size())
                    )
                );
            }
        }
        return result;
    }

    /**
     * Render a node vertically: the head on this line, each argument
     * laid out on the lines below, indented one level deeper.
     * @param node The node
     * @param indent The indentation level
     * @return The rendered block
     */
    private String vertical(final Pretty.Node node, final int indent) {
        final StringBuilder block = new StringBuilder(this.step().repeat(indent))
            .append(node.base)
            .append(node.tail);
        for (final Pretty.Node child : node.children) {
            if (child.test) {
                block.append('\n');
            }
            block.append('\n').append(this.layout(child, indent + 1));
        }
        return block.toString();
    }

    /**
     * Render a node horizontally on a single line, if all of its
     * arguments can be inlined.
     * @param node The node
     * @param indent The indentation level
     * @return The single line, or empty if inlining is impossible
     */
    private Optional<String> horizontal(final Pretty.Node node, final int indent) {
        final Optional<String> result;
        if (node.abstractt) {
            result = this.phi(node, indent);
        } else if (node.children.isEmpty()) {
            result = Optional.empty();
        } else {
            result = Pretty.inlined(node.children).map(
                args -> new StringBuilder(this.step().repeat(indent))
                    .append(node.base)
                    .append(' ')
                    .append(args)
                    .append(node.tail)
                    .toString()
            );
        }
        return result;
    }

    /**
     * Render a formation in the compact inline-phi form, when its only
     * attribute is the {@code φ} decoratee.
     *
     * <p>A formation with a {@code [params]} head collapses to
     * {@code <phi> > [params] > name} (R-3.10.8 §4.5). A test attribute
     * with no void params has an empty head — the head template renders
     * it through the {@code ++> name} shorthand — so it collapses to
     * {@code <phi> ++> name} (R-3.10.8 / R-6.3.6, issue #5567), the
     * decoratee sitting in front of the {@code ++>} marker instead of a
     * bracket. An empty head occurs only for that no-void test attribute,
     * so it selects the shorthand separator here.</p>
     *
     * <p>This applies only when {@code @} is the sole binding; a
     * formation with any other attribute keeps the vertical layout. The
     * decoratee itself is inlined through the usual {@link #flat} path,
     * so a decoratee that can't be inlined (a nested formation, a tuple)
     * yields empty here; the caller then falls back to either the
     * {@link #hybrid} form (marker compacted, arguments vertical) or the
     * fully verbose vertical shape, whichever the penalty/width check
     * prefers.</p>
     *
     * @param node The formation node
     * @param indent The indentation level
     * @return The single line, or empty if the inline-phi form doesn't apply
     */
    private Optional<String> phi(final Pretty.Node node, final int indent) {
        final Optional<String> result;
        if (Pretty.decorated(node)) {
            final Pretty.Node decoratee = node.children.get(0);
            result = Pretty.flat(
                new Pretty.Node(
                    decoratee.base, "", decoratee.abstractt,
                    false, decoratee.reversed, decoratee.data, decoratee.children
                )
            ).map(
                value -> new StringBuilder(this.step().repeat(indent))
                    .append(value)
                    .append(Pretty.marker(node))
                    .toString()
            );
        } else {
            result = Optional.empty();
        }
        return result;
    }

    /**
     * Render a formation in the hybrid inline-phi form: the decoratee's
     * head sitting in front of the marker, with the decoratee's arguments
     * laid out vertically below.
     *
     * <p>This is the multi-line counterpart of {@link #phi}. When the
     * decoratee has arguments that cannot all be inlined onto one line
     * (a tuple, a nested formation), {@link #phi} yields empty and the
     * fully-inline single line is unavailable. The verbose fallback then
     * repeats the marker on its own line and pushes the decoratee a level
     * deeper ({@code ++> name} / {@code > name} above {@code head > @}
     * above the arguments). This form instead keeps the compact marker —
     * {@code head ++> name} (or {@code head > [params] > name}) — and hangs
     * the arguments directly beneath, mirroring the ordinary
     * {@code head > name} plus vertical-args layout, saving one line and
     * one indent level (issue #5594). It is offered as a candidate; the
     * penalty search keeps it only when it beats the verbose vertical
     * shape.</p>
     *
     * <p>It applies only to a decoratee with children that is not a
     * formation and is not a receiver-only reversed dispatch: a childless
     * decoratee already inlines through {@link #phi}, a formation
     * decoratee lays its bindings out vertically rather than as arguments,
     * and a reversed dispatch with just its receiver keeps the verbose
     * layout (mirroring the receiver-only rejection in {@link #flat}), so
     * none of them has a hybrid form. A reversed dispatch that also carries
     * arguments ({@code if.} with its branches) does, laid out with the
     * receiver and arguments beneath, exactly as its verbose {@code > @}
     * body already was.</p>
     *
     * @param node The formation node
     * @param indent The indentation level
     * @return The rendered block, or empty if the hybrid form doesn't apply
     */
    private Optional<String> hybrid(final Pretty.Node node, final int indent) {
        Optional<String> result = Optional.empty();
        if (Pretty.decorated(node)) {
            final Pretty.Node decoratee = node.children.get(0);
            if (!decoratee.abstractt && !decoratee.children.isEmpty()
                && !(decoratee.reversed && decoratee.children.size() <= 1)) {
                result = Optional.of(
                    this.vertical(
                        new Pretty.Node(
                            decoratee.base, Pretty.marker(node), false, false,
                            decoratee.reversed, decoratee.data, decoratee.children
                        ),
                        indent
                    )
                );
            }
        }
        return result;
    }

    /**
     * Whether this node is a formation whose only binding is the
     * {@code φ} decoratee, and so is a candidate for the compact
     * inline-phi renderings.
     * @param node The node
     * @return True if the node has a lone {@code > @} decoratee
     */
    private static boolean decorated(final Pretty.Node node) {
        return node.abstractt
            && node.children.size() == 1
            && " > @".equals(node.children.get(0).tail);
    }

    /**
     * The compact inline-phi marker that trails the decoratee: the piece
     * that replaces the decoratee's own {@code > @} suffix.
     *
     * <p>An empty formation head (a no-void test attribute) renders through
     * the {@code ++> name} shorthand, already carried in the node's tail, so
     * only a separating space is prepended. A {@code [params]} head is
     * spliced in as {@code > [params]} before the {@code > name} tail.</p>
     *
     * @param node The formation node
     * @return The trailing marker string
     */
    private static String marker(final Pretty.Node node) {
        final String middle;
        if (node.base.isEmpty()) {
            middle = " ";
        } else {
            middle = " > ".concat(node.base);
        }
        return middle.concat(node.tail);
    }

    /**
     * Inline a list of arguments into one space-separated string,
     * wrapping in parentheses only those that apply arguments of their
     * own (a real application such as {@code 5.plus 3}).
     *
     * <p>The guard weighs the argument in its effective (suffix-resolved)
     * shape, not its raw one. A data-receiver dispatch such as
     * {@code 01-.as-bool} is stored as a reversed head over a data child,
     * so its raw node has a child (the receiver) yet it takes no arguments
     * and is a single token — {@link #suffixed} folds the receiver back
     * into the base, leaving no children, so it inlines bare. Wrapping it
     * as {@code (01-.as-bool)} would produce EO that fails to parse with
     * "redundant parentheses around a single token" (#5591).</p>
     *
     * @param args The arguments
     * @return The inlined string, or empty if any argument can't be inlined
     */
    private static Optional<String> inlined(final List<Pretty.Node> args) {
        final StringBuilder joined = new StringBuilder();
        Optional<String> result = Optional.of("");
        for (final Pretty.Node arg : args) {
            final Optional<String> flat = Pretty.flat(arg);
            if (flat.isEmpty()) {
                result = Optional.empty();
                break;
            }
            if (joined.length() > 0) {
                joined.append(' ');
            }
            if (Pretty.suffixed(arg).orElse(arg).children.isEmpty()) {
                joined.append(flat.get());
            } else {
                joined.append('(').append(flat.get()).append(')');
            }
        }
        return result.map(ignored -> joined.toString());
    }

    /**
     * Render a node inline, as it would appear as an argument (without
     * its own name suffix), or empty if it can't be inlined safely. A
     * data-receiver dispatch is inlined in its suffix shape ({@code
     * 5.plus 3}), never the reversed one.
     * @param given The node
     * @return The inlined content, or empty
     */
    private static Optional<String> flat(final Pretty.Node given) {
        final Optional<String> result;
        final Pretty.Node node = Pretty.suffixed(given).orElse(given);
        if (node.reversed && node.children.size() <= 1) {
            result = Optional.empty();
        } else if (node.abstractt || !node.tail.isEmpty() || "*".equals(node.base)) {
            result = Optional.empty();
        } else if (node.children.isEmpty()) {
            result = Optional.of(node.base);
        } else {
            result = Pretty.inlined(node.children)
                .map(args -> String.join(" ", node.base, args));
        }
        return result;
    }

    /**
     * A node of the intermediate line tree.
     * @since 0.57.0
     */
    private static final class Node {

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
        private final List<Pretty.Node> children;

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
            final List<Pretty.Node> kids) {
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
        static Pretty.Node parse(final Xnav line) {
            return new Pretty.Node(
                line.attribute("base").text().orElse(""),
                line.attribute("tail").text().orElse(""),
                "yes".equals(line.attribute("abstract").text().orElse("no")),
                "yes".equals(line.attribute("test").text().orElse("no")),
                "yes".equals(line.attribute("reversed").text().orElse("no")),
                "yes".equals(line.attribute("data").text().orElse("no")),
                line.elements(Filter.withName("line"))
                    .map(Pretty.Node::parse)
                    .collect(Collectors.toList())
            );
        }
    }
}
