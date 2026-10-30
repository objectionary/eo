/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import org.xembly.Directives;

/**
 * Every call the program makes, with what it passes.
 *
 * <p>An application is the one place in the text where a void stops being a
 * question, and what it passes is there to be read before anything is worked
 * out: in {@code inc t} the application at {@code Φ.app.φ} puts the object at
 * {@code Φ.app.φ.α0} into its first place. That is all this section holds,
 * one {@code call} per application and one {@code arg} per argument, into
 * the {@code calls} section of the facts:</p>
 *
 * <pre> &lt;call loc="Φ.app.φ"&gt;
 *   &lt;arg n="0" loc="Φ.app.φ.α0"/&gt;
 * &lt;/call&gt;</pre>
 *
 * <p>An argument goes into a place by what its {@code @as} says, not by where
 * it sits among its siblings: an inline binding such as {@code pair 5:1} can
 * leave an earlier place for whoever applies next, and that place is not
 * written at all, since nothing in the code stands there: {@code pair 5:1}
 * is a call with one {@code arg}, at {@code n="1"}. A binding may name the
 * void itself rather than its place, and is written by that name instead
 * (R-3.12.1), and a call that passes
 * everything by name, as {@code pick oak:m} or {@code run y:^} do, is written
 * with no place at all, only its named arguments (R-3.12.2a).</p>
 *
 * <p>Which void a place is, and what object the call copies, are answers and
 * stay out. They are worked out later from the links, and a call whose copy
 * is never worked out is still a call: written down here, it leaves a trace
 * for whoever asks about it.</p>
 *
 * @since 0.69.0
 */
final class Applications implements Section {

    @Override
    public Directives directives(final Path xmirs) throws IOException {
        final Directives dirs = new Directives().add("calls");
        for (final XML application : new Xmirs(xmirs).applications()) {
            final Xnav node = new Xnav(application.inner());
            final List<Xnav> args = node.elements(Filter.withName("o")).toList();
            dirs.add("call").attr("loc", node.attribute("loc").text().orElse(""));
            final List<Xnav> placed = args.stream().filter(
                arg -> arg.attribute("loc").text().isPresent()
                    && arg.attribute("as").text().orElse("").startsWith("α")
            ).sorted(
                Comparator.comparingInt(
                    arg -> Integer.parseInt(arg.attribute("as").text().get().substring(1))
                )
            ).toList();
            for (final Xnav arg : placed) {
                dirs.add("arg")
                    .attr("n", arg.attribute("as").text().get().substring(1))
                    .attr("loc", arg.attribute("loc").text().get())
                    .up();
            }
            for (final Xnav arg : args) {
                final Optional<String> name = arg.attribute("as").text();
                final Optional<String> loc = arg.attribute("loc").text();
                if (name.isPresent() && loc.isPresent() && !name.get().startsWith("α")) {
                    dirs.add("arg").attr("name", name.get()).attr("loc", loc.get()).up();
                }
            }
            dirs.up();
        }
        return dirs.up();
    }
}
