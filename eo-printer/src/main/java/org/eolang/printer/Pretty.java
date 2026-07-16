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
     * @param node The node
     * @param indent The indentation level
     * @return The rendered block
     */
    private String layout(final Pretty.Node node, final int indent) {
        String best = this.vertical(node, indent);
        final Optional<String> flat = this.horizontal(node, indent);
        if (flat.isPresent()
            && new Penalty(flat.get(), this.weights).points()
            < new Penalty(best, this.weights).points()) {
            best = flat.get();
        }
        return best;
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
     * yields empty and the caller falls back to the vertical shape; the
     * penalty/width check then decides whether this single line is
     * actually preferable.</p>
     *
     * @param node The formation node
     * @param indent The indentation level
     * @return The single line, or empty if the inline-phi form doesn't apply
     */
    private Optional<String> phi(final Pretty.Node node, final int indent) {
        final Optional<String> result;
        if (node.children.size() == 1
            && " > @".equals(node.children.get(0).tail)) {
            final Pretty.Node decoratee = node.children.get(0);
            final String middle;
            if (node.base.isEmpty()) {
                middle = " ";
            } else {
                middle = " > ".concat(node.base);
            }
            result = Pretty.flat(
                new Pretty.Node(
                    decoratee.base, "", decoratee.abstractt,
                    false, decoratee.reversed, decoratee.children
                )
            ).map(
                value -> new StringBuilder(this.step().repeat(indent))
                    .append(value)
                    .append(middle)
                    .append(node.tail)
                    .toString()
            );
        } else {
            result = Optional.empty();
        }
        return result;
    }

    /**
     * Inline a list of arguments into one space-separated string,
     * wrapping in parentheses those that take arguments themselves.
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
            if (arg.children.isEmpty()) {
                joined.append(flat.get());
            } else {
                joined.append('(').append(flat.get()).append(')');
            }
        }
        return result.map(ignored -> joined.toString());
    }

    /**
     * Render a node inline, as it would appear as an argument (without
     * its own name suffix), or empty if it can't be inlined safely.
     * @param node The node
     * @return The inlined content, or empty
     */
    private static Optional<String> flat(final Pretty.Node node) {
        final Optional<String> result;
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
         * @param kids The children
         * @checkstyle ParameterNumberCheck (5 lines)
         */
        Node(final String head, final String suffix, final boolean formation,
            final boolean attr, final boolean rev, final List<Pretty.Node> kids) {
            this.base = head;
            this.tail = suffix;
            this.abstractt = formation;
            this.test = attr;
            this.reversed = rev;
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
                line.elements(Filter.withName("line"))
                    .map(Pretty.Node::parse)
                    .collect(Collectors.toList())
            );
        }
    }
}
