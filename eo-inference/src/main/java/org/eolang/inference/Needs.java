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
import org.xembly.Directives;

/**
 * Every read of the program, one row per read.
 *
 * <p>This is the other half of {@link Provides}: a formation shows what its
 * object certainly has, while a dispatch shows what somebody expects to find.
 * In this fragment</p>
 *
 * <pre> &lt;o base=".next" loc="Φ.app.inc.φ.ρ"&gt;
 *   &lt;o base=".x" loc="Φ.app.inc.φ.ρ.ρ"&gt;
 *     &lt;o base="ξ" loc="Φ.app.inc.φ.ρ.ρ.ρ"/&gt;
 *   &lt;/o&gt;
 * &lt;/o&gt;</pre>
 *
 * <p>somebody asks for {@code next} off the place {@code Φ.app.inc.φ.ρ.ρ},
 * and the result lands at {@code Φ.app.inc.φ.ρ}. That is one row, about the
 * place the result lands at:</p>
 *
 * <pre> &lt;read id="Φ.app.inc.φ.ρ" name="next" of="Φ.app.inc.φ.ρ.ρ"/&gt;</pre>
 *
 * <p>A read is a question, not an attribute: nothing here says the receiver
 * has {@code next}, only that somebody asks it for one. The {@code .foo}
 * wrapped around this fragment adds another row, asking the dispatch for
 * {@code foo} in turn — the chain is walked simply by every step of it being
 * a dispatch of its own, which is what the splitting done before any clue
 * looks at the XMIR is for.</p>
 *
 * <p>A row is about a place in the program, not about an object. Which places
 * hold the same object is not known here; the links work that out later, and
 * {@code Demanded} turns what is read off a void into a demand on whatever
 * fills it. So two rows that look alike, with the same name read off the same
 * head, are not duplicates: thousands of rows read {@code bytes} off a bare
 * {@code Φ}, and each one is the only fact about its own place.</p>
 *
 * <p>The receiver is the child that is not an argument: {@code x.plus 5}
 * takes {@code plus} from {@code x}, never from the {@code 5}. The parser
 * marks every argument with {@code as="αN"}, so the receiver is the one child
 * without an {@code @as}, and it is always there: a name written by itself
 * gets a bare {@code ξ} as its receiver, a name the whole program knows gets
 * a bare {@code Φ}, and every other name gets the read it is written after.
 * Nothing here
 * tries to work out what the receiver actually is, or whether it really has
 * the name — asking is all this section does. Comparing the answer with
 * what {@link Provides} wrote down is the job of the checking loop, once the
 * links between copies are known.</p>
 *
 * <p>This is the one place that reads the dispatches off the XMIR, and the
 * rows go into the {@code reads} section of the facts. Everything after it
 * reads them from there, each as a {@code Site}, in the order the dispatches
 * appear in the code: the rows keep that order, and the answers depend on
 * it.</p>
 *
 * @since 0.68.0
 */
final class Needs implements Section {

    @Override
    public Directives directives(final Path xmirs) throws IOException {
        final Directives dirs = new Directives().add("reads");
        for (final XML node : new Xmirs(xmirs).dispatches()) {
            final Xnav dispatch = new Xnav(node.inner());
            final String base = new Noted(dispatch).says("base");
            dirs.add("read")
                .attr("id", new Noted(dispatch).says("loc"))
                .attr("name", base.substring(base.indexOf('.') + 1))
                .attr("of", Needs.bearer(dispatch))
                .up();
        }
        return dirs.up();
    }

    private static String bearer(final Xnav dispatch) {
        return dispatch
            .elements(Filter.all(Filter.withName("o"), Filter.not(Filter.hasAttribute("as"))))
            .findFirst()
            .map(kid -> new Noted(kid).says("loc")).orElseThrow(
                () -> new IllegalStateException(
                    String.format(
                        "The dispatch at %s has no receiver",
                        new Noted(dispatch).says("loc")
                    )
                )
            );
    }
}
