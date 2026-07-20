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

/**
 * Penalty-based pretty printer of EO code.
 *
 * <p>It takes the intermediate "line tree" produced by
 * {@code to-eo-tree.xsl} (an {@code <eo>} element with a {@code
 * <preamble>} and nested {@code <line>} elements) and lays it out as
 * EO source. For every object it considers a few renderings — a
 * vertical one, where the arguments go on their own indented lines, a
 * horizontal one, where they are inlined (wrapped in parentheses when
 * needed), and, for a tuple applied at the tail, a hybrid that carries
 * the {@code *N} compact-tuple marker on the head's line — and keeps the
 * one with the smaller {@link Penalty}. The decision is made recursively,
 * bottom-up. The one exception to the penalty vote is the {@code *N}
 * marker for {@code N >= 1} leading arguments (issue #5648): being the
 * canonical spelling of a tuple applied after positional arguments, it is
 * always emitted, never the verbose {@code * elem} child.</p>
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
     * A single level of indentation, whose width is the {@code STEP} weight.
     */
    private final String tab;

    /**
     * The column past which a line overflows, the {@code WIDTH} weight.
     */
    private final int width;

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
     * @checkstyle ConstructorsCodeFreeCheck (12 lines)
     */
    Pretty(final Xnav element, final Map<PenaltyKey, Integer> config) {
        this.root = element;
        this.weights = config;
        this.tab = " ".repeat(
            config.getOrDefault(PenaltyKey.STEP, PenaltyKey.STEP.fallback())
        );
        this.width = config.getOrDefault(
            PenaltyKey.WIDTH, PenaltyKey.WIDTH.fallback()
        );
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
            .map(line -> this.layout(Node.parse(line), 0))
            .ifPresent(out::append);
        return out.append('\n').toString();
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
    private String layout(final Node node, final int indent) {
        String best = this.shaped(node, indent);
        final Optional<Node> suffix = Pretty.suffixed(node);
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
     * vertical, horizontal and trailing-star renderings.
     *
     * <p>The compact-tuple {@code *N} marker for {@code N >= 1} leading
     * arguments (issue #5648) is an exception to the penalty vote: it is the
     * canonical spelling of a tuple applied after positional arguments, so it
     * is emitted whenever it applies, even when the verbose {@code * elem}
     * child inlines onto fewer lines. The bare {@code *} idiom
     * ({@code N == 0}) stays a mere candidate — a short tuple that fits
     * inline as {@code seq}, {@code * 1 2} on the next line is left alone.</p>
     *
     * @param node The node
     * @param indent The indentation level
     * @return The rendered block
     */
    private String shaped(final Node node, final int indent) {
        final Optional<String> star = this.starred(node, indent);
        final String result;
        if (star.isPresent() && node.children.size() > 1) {
            result = star.get();
        } else {
            String best = this.vertical(node, indent);
            final Optional<String> flat = this.horizontal(node, indent);
            if (flat.isPresent()
                && new Penalty(flat.get(), this.weights).points()
                < new Penalty(best, this.weights).points()) {
                best = flat.get();
            }
            if (star.isPresent()
                && new Penalty(star.get(), this.weights).points()
                < new Penalty(best, this.weights).points()) {
                best = star.get();
            }
            result = best;
        }
        return result;
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
    private static Optional<Node> suffixed(final Node node) {
        Optional<Node> result = Optional.empty();
        if (node.reversed && !node.children.isEmpty()) {
            final Node receiver = node.children.get(0);
            if (receiver.data && receiver.children.isEmpty()
                && receiver.tail.isEmpty()) {
                result = Optional.of(
                    new Node(
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
    private String vertical(final Node node, final int indent) {
        final StringBuilder block = new StringBuilder(this.tab.repeat(indent))
            .append(node.base)
            .append(node.tail);
        for (final Node child : node.children) {
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
    private Optional<String> horizontal(final Node node, final int indent) {
        final Optional<String> result;
        if (node.abstractt) {
            result = this.phi(node, indent);
        } else if (node.children.isEmpty()) {
            result = Optional.empty();
        } else {
            result = Pretty.inlined(node.children).map(
                args -> new StringBuilder(this.tab.repeat(indent))
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
     * decoratee itself is inlined through the usual {@link #flat} path.
     * When that fails because the decoratee's arguments must go vertical (a
     * tuple, a nested formation), a hybrid multi-line form is returned
     * instead: the decoratee's head kept in front of the marker
     * ({@code head ++> name} or {@code head > [params] > name}) with the
     * arguments laid out beneath, mirroring the ordinary {@code head > name}
     * plus vertical-args layout and saving one line and one indent level
     * over the verbose shape (issue #5594). The flat one-liner is kept while
     * it fits the {@code WIDTH} limit, but once it overflows (or cannot be
     * built) the hybrid is used instead, rather than gating the hybrid behind
     * the one-liner's absence and falling back to the verbose shape when the
     * one-liner overflows (issue #5635). Either way the result is only a
     * candidate — the penalty/width check in {@link #shaped} keeps it only
     * when it beats the plain vertical rendering. A formation decoratee
     * (its bindings are vertical, not arguments) and a receiver-only
     * reversed dispatch ({@code not.} with just its receiver, mirroring the
     * rejection in {@link #flat}) have no hybrid form, so they yield empty
     * and keep the verbose layout; a reversed dispatch that also carries
     * arguments ({@code if.} with its branches) does get the hybrid.</p>
     *
     * <p>The hybrid is also withheld when any line in the decoratee's whole
     * subtree carries a name suffix ({@code [left] >>}, {@code malloc.for >
     * [b]}, {@code b.put > [m] >>}). The decoratee's subtree becomes the body
     * of the collapsed only-phi formation, and an only-phi formation may hold
     * nothing but its {@code φ} decoratee, so a named line anywhere within it
     * fails to parse with "an auto-named attribute cannot be a named attribute
     * of an only-phi formation, which binds only its φ decoratee" (issues
     * #5598, #5604). A direct child is not enough to check: the offending line
     * may be nested deeper — inside a tuple, a dispatch, or an application —
     * where a shallow guard would miss it (issue #5604). Keeping the verbose
     * {@code  > @} layout gives the decoratee its own scope, where named
     * arguments are legal.</p>
     *
     * @param node The formation node
     * @param indent The indentation level
     * @return The rendered block, or empty if the inline-phi form doesn't apply
     */
    private Optional<String> phi(final Node node, final int indent) {
        Optional<String> result = Optional.empty();
        if (node.children.size() == 1
            && " > @".equals(node.children.get(0).tail)) {
            final Node decoratee = node.children.get(0);
            final String middle;
            if (node.base.isEmpty()) {
                middle = " ";
            } else {
                middle = " > ".concat(node.base);
            }
            final String marker = middle.concat(node.tail);
            final Optional<String> flat = Pretty.flat(
                new Node(
                    decoratee.base, "", decoratee.abstractt,
                    false, decoratee.reversed, decoratee.data, decoratee.children
                )
            ).map(
                inlined -> this.tab.repeat(indent).concat(inlined).concat(marker)
            );
            final boolean applied = !decoratee.abstractt && !decoratee.children.isEmpty()
                && !(decoratee.reversed && decoratee.children.size() <= 1);
            final boolean unnamed = decoratee.children.stream()
                .allMatch(Node::nameless);
            if (applied && unnamed
                && flat.map(line -> line.length() > this.width).orElse(true)) {
                result = Optional.of(
                    this.vertical(
                        new Node(
                            decoratee.base, marker, false, false,
                            decoratee.reversed, decoratee.data, decoratee.children
                        ),
                        indent
                    )
                );
            } else {
                result = flat;
            }
        }
        return result;
    }

    /**
     * Render an application whose last child is a tuple as a hybrid: the
     * head carrying a trailing {@code *N} marker on one line, with every
     * argument laid out vertically beneath at one deeper indent.
     *
     * <p>The ordinary {@code seq *} idiom is a tuple applied as the sole
     * argument of an object. Rendered verbosely it becomes {@code seq} on
     * one line, a lone {@code *} on the next, and the elements one level
     * deeper still — a line taller and an indent wider than the source a
     * human writes. This keeps the {@code *} at the tail of the head's line
     * ({@code seq *}) and pulls the elements up one level, mirroring the
     * hybrid inline-phi form (issues #5594, #5615). When the tuple follows
     * {@code N >= 1} leading positional arguments, the compact-tuple marker
     * {@code *N} (§3.9) carries the count on the head's line
     * ({@code sprintf *1}) and every argument — the leading ones and the
     * tuple's elements alike — becomes an indented sibling (issue #5648).
     * Either way the shape is the same tree as the verbose one, with no
     * before-star ambiguity.</p>
     *
     * <p>It applies to a plain (non-formation, non-reversed) application
     * whose last child is a non-empty, unnamed star. When the star is the
     * sole child ({@code N == 0}), the bare {@code *} absorbs the indented
     * siblings into the tuple and the head must be a plain base, not a
     * dotted method dispatch: after {@code "literal".printf *} the parser
     * reads a complete application with an empty tuple and drops the
     * indented element, so {@link #tuply()} bars that case (issues #5622,
     * #5624). The {@code *N} marker ({@code N >= 1}) sits on the head line
     * rather than being glued after arguments, so it round-trips after a
     * dotted dispatch too ({@code string.sprintf *1}). The genuinely
     * ambiguous before-star form {@code head args *} is never produced. The
     * result is only a candidate: {@link #shaped} keeps it only when its
     * penalty beats the plain vertical and horizontal renderings, so a short
     * tuple that fits inline as {@code seq}, {@code * 1 2} on the next line
     * is left alone.</p>
     *
     * @param node The node
     * @param indent The indentation level
     * @return The rendered block, or empty if the trailing-star form doesn't apply
     */
    private Optional<String> starred(final Node node, final int indent) {
        Optional<String> result = Optional.empty();
        if (node.tuply()) {
            result = Optional.of(this.vertical(node.glued(), indent));
        }
        return result;
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
    private static Optional<String> inlined(final List<Node> args) {
        final StringBuilder joined = new StringBuilder();
        Optional<String> result = Optional.of("");
        for (final Node arg : args) {
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
    private static Optional<String> flat(final Node given) {
        final Optional<String> result;
        final Node node = Pretty.suffixed(given).orElse(given);
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
}
