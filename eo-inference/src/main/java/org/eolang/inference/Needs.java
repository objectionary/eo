/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjDeferred;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * What every object must have, judging by how it is used.
 *
 * <p>This is the other half of {@link Provides}: a formation shows what its
 * object certainly has, while a dispatch shows what somebody expects to find.
 * In this fragment</p>
 *
 * <pre> &lt;o base=".next" loc="Φ.app.inc.φ.ρ"&gt;
 *   &lt;o base="ξ.x" loc="Φ.app.inc.φ.ρ.ρ"/&gt;
 * &lt;/o&gt;</pre>
 *
 * <p>somebody takes {@code next} from the object at
 * {@code Φ.app.inc.φ.ρ.ρ}, so that object must have such an attribute, and
 * whatever it holds is the type of the dispatch itself. That is one row. The
 * {@code .foo} wrapped around this fragment adds another, asking the dispatch
 * for {@code foo} in turn — the chain is walked simply by every step of it
 * being a dispatch of its own, which is what the splitting done before any
 * clue looks at the XMIR is for.</p>
 *
 * <p>The receiver is the child that is not an argument: {@code x.plus 5}
 * takes {@code plus} from {@code x}, never from the {@code 5}. Nothing here
 * tries to work out what the receiver actually is, or whether it really has
 * the attribute — asking is all this clue does. Comparing the answer with
 * what {@link Provides} wrote down is the job of the checking loop, once the
 * links between copies are known.</p>
 *
 * @since 0.68.0
 */
final class Needs implements Clue {

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            for (final Site dispatch : new Xmirs(xmirs).dispatches()) {
                final String owner = dispatch.bearer();
                rows.add(owner);
                rows.add(String.join(" ", owner, dispatch.name()))
                    .set("owner", owner)
                    .set("name", dispatch.name())
                    .set("type", dispatch.made());
            }
            Files.createDirectories(tables);
            Files.write(
                tables.resolve("needs.xml"),
                new Grouped(rows, "needs").asXml().toString().getBytes(StandardCharsets.UTF_8)
            );
        }
    }
}
