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
 * EO source. For every object it considers a few renderings — a
 * vertical one, where the arguments go on their own indented lines, a
 * horizontal one, where they are inlined (wrapped in parentheses when
 * needed), and, for a tuple applied at the tail, a hybrid that glues
 * the {@code *} to the head's line — and keeps the one with the
 * smaller {@link Penalty}. The decision is made recursively,
 * bottom-up.</p>
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
            .map(line -> this.layout(Pretty.Node.parse(line), 0))
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
     * vertical, horizontal and trailing-star renderings.
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
        }
        final Optional<String> star = this.starred(node, indent);
        if (star.isPresent()
            && new Penalty(star.get(), this.weights).points()
            < new Penalty(best, this.weights).points()) {
            best = star.get();
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
        final StringBuilder block = new StringBuilder(this.tab.repeat(indent))
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
     * over the verbose shape (issue #5594); when those arguments are a lone
     * tuple the star is glued onto the head line too ({@link #hybrid}). The
     * flat one-liner is kept while it fits the {@code WIDTH} limit, but once
     * it overflows (or cannot be built) the hybrid is used instead, rather
     * than gating the hybrid behind the one-liner's absence and falling back
     * to the verbose shape when the one-liner overflows (issue #5635). Either
     * way the result is only a candidate — the penalty/width check in
     * {@link #shaped} keeps it only when it beats the plain vertical
     * rendering. A formation decoratee
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
    private Optional<String> phi(final Pretty.Node node, final int indent) {
        Optional<String> result = Optional.empty();
        if (node.children.size() == 1
            && " > @".equals(node.children.get(0).tail)) {
            final Pretty.Node decoratee = node.children.get(0);
            final String middle;
            if (node.base.isEmpty()) {
                middle = " ";
            } else {
                middle = " > ".concat(node.base);
            }
            final String marker = middle.concat(node.tail);
            final Optional<String> flat = Pretty.flat(
                new Pretty.Node(
                    decoratee.base, "", decoratee.abstractt,
                    false, decoratee.reversed, decoratee.data, decoratee.children
                )
            ).map(
                inlined -> this.tab.repeat(indent).concat(inlined).concat(marker)
            );
            final boolean applied = !decoratee.abstractt && !decoratee.children.isEmpty()
                && !(decoratee.reversed && decoratee.children.size() <= 1);
            final boolean unnamed = decoratee.children.stream()
                .allMatch(Pretty.Node::nameless);
            if (applied && unnamed
                && flat.map(line -> line.length() > this.width).orElse(true)) {
                result = Optional.of(
                    this.vertical(decoratee.hybrid(marker), indent)
                );
            } else {
                result = flat;
            }
        }
        return result;
    }

    /**
     * Render an application whose last child is a tuple as a hybrid: the
     * head glued to a trailing {@code *} on one line, with the tuple's
     * elements laid out vertically beneath at one deeper indent.
     *
     * <p>The ordinary {@code seq *} idiom is a tuple applied as the last
     * argument of an object. Rendered verbosely it becomes {@code seq} on
     * one line, a lone {@code *} on the next, and the elements one level
     * deeper still — a line taller and an indent wider than the source a
     * human writes. This keeps the {@code *} at the tail of the head's line
     * ({@code seq *}) and pulls the elements up one level, mirroring the
     * hybrid inline-phi form (issues #5594, #5615). A trailing {@code *}
     * absorbs exactly the indented siblings beneath it into the tuple, so
     * this shape is the same tree as the verbose one, with no before-star
     * ambiguity.</p>
     *
     * <p>It applies only to a plain (non-formation, non-reversed)
     * application whose sole child is a non-empty, unnamed star. The star
     * must be the only child: a bare trailing {@code *} absorbs the
     * indented siblings into the tuple, so the shape round-trips only when
     * nothing else shares the head's line. A preceding argument
     * ({@code sm.win32 "getenv" *}) would make the parser read a complete
     * application with an empty tuple and reject the indented child, so
     * {@link #tuply()} bars that case (issue #5622). The elements are always
     * laid out vertically, so the genuinely ambiguous fully-horizontal
     * {@code head * a b} — the before-star territory — is never produced
     * here. The result is only a candidate: {@link #shaped} keeps it only
     * when its penalty beats the plain vertical and horizontal renderings,
     * so a short tuple that fits inline as {@code seq}, {@code * 1 2} on the
     * next line is left alone.</p>
     *
     * @param node The node
     * @param indent The indentation level
     * @return The rendered block, or empty if the trailing-star form doesn't apply
     */
    private Optional<String> starred(final Pretty.Node node, final int indent) {
        Optional<String> result = Optional.empty();
        if (node.tuply()) {
            result = Optional.of(
                this.vertical(node.children.get(0).glued(node), indent)
            );
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
                && this.children.stream().allMatch(Pretty.Node::nameless);
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
         * but {@link #suffixed} rebuilds it as a non-reversed, single-child
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
        Pretty.Node glued(final Pretty.Node applier) {
            return new Pretty.Node(
                String.join(" ", applier.base, this.base), applier.tail,
                false, false, false, false, this.children
            );
        }

        /**
         * Build the body of the hybrid inline-phi form for this decoratee: its
         * head kept in front of {@code marker}, its arguments as children.
         *
         * <p>When this decoratee applies a lone tuple ({@code seq *}), the
         * trailing {@code *} is glued onto that head line ({@code seq * > [m]})
         * through {@link #glued} and the tuple's elements become the children,
         * mirroring the {@code starred} idiom and saving a line and an indent
         * level; the parser absorbs a compact tuple in inline-phi position, so
         * this round-trips (issue #5626). Otherwise the arguments stay as this
         * node's children, laid out vertically by the caller.</p>
         *
         * @param marker The inline-phi marker ({@code  > [params] > name})
         * @return The body node to lay out vertically
         */
        Pretty.Node hybrid(final String marker) {
            final Pretty.Node plain = new Pretty.Node(
                this.base, marker, false, false,
                this.reversed, this.data, this.children
            );
            final Pretty.Node body;
            if (this.tuply()) {
                body = this.children.get(0).glued(plain);
            } else {
                body = plain;
            }
            return body;
        }
    }
}
