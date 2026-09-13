/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;
import org.w3c.dom.Node;

/**
 * Test case for {@link Lowered}.
 *
 * <p>The tests run the real binary and hold only when it is installed
 * and of the pinned version, which is what CI arranges.</p>
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
@DisabledOnOs(OS.WINDOWS)
final class LoweredTest {

    @Test
    void lowersNestedFragmentIntoAtom(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino("phino", 500, temp.resolve("phino"));
        Assumptions.assumeTrue(phino.suitable());
        final Home home = new Home(temp.resolve("lower"));
        final Formas formas = LoweredTest.formas();
        final Node doc = LoweredTest.prepared(temp, home, formas);
        new Lowered(phino, formas, home, "foo").rewrite(new Xnav(doc));
        MatcherAssert.assertThat(
            "the nested fragment must come back as an atom over its void, but it didnt",
            new Xml(doc).text(),
            Matchers.allOf(
                Matchers.matchesPattern("(?s).*lowered=\"[0-9a-f]{12}\" name=\"f\".*"),
                Matchers.containsString("<o atom=\"Φ.number\" name=\"λ\"/>")
            )
        );
    }

    @Test
    void countsLoweredFragments(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino("phino", 500, temp.resolve("phino"));
        Assumptions.assumeTrue(phino.suitable());
        final Home home = new Home(temp.resolve("lower"));
        final Formas formas = LoweredTest.formas();
        MatcherAssert.assertThat(
            "the one fragment of the document must be counted, but it wasnt",
            new Lowered(phino, formas, home, "foo").rewrite(
                new Xnav(LoweredTest.prepared(temp, home, formas))
            ),
            Matchers.equalTo(1)
        );
    }

    @Test
    void writesSidecarOfTheAtom(@Mktmp final Path temp) throws IOException {
        final Phino phino = new Phino("phino", 500, temp.resolve("phino"));
        Assumptions.assumeTrue(phino.suitable());
        final Home home = new Home(temp.resolve("lower"));
        final Formas formas = LoweredTest.formas();
        new Lowered(phino, formas, home, "foo").rewrite(
            new Xnav(LoweredTest.prepared(temp, home, formas))
        );
        try (Stream<Path> files = Files.list(home.atoms())) {
            MatcherAssert.assertThat(
                "the sidecar must add the literal to the void in Java, but it doesnt",
                Files.readString(files.findFirst().get(), StandardCharsets.UTF_8),
                Matchers.containsString("v0 + Double.longBitsToDouble(0x4014000000000000L)")
            );
        }
    }

    private static Formas formas() {
        return new Formas(
            Collections.emptyMap(), Collections.singletonMap("Φ.foo.f.a", "number")
        );
    }

    private static Node prepared(final Path temp, final Home home, final Formas formas)
        throws IOException {
        final Map<String, String> docs = new LinkedHashMap<>(3);
        docs.put(
            "number",
            String.join(
                "",
                "<o loc='Φ.number' name='number'><o base='∅' loc='Φ.number.φ' name='φ'/>",
                "<o loc='Φ.number.plus' name='plus'><o base='∅' loc='Φ.number.plus.ρ' name='ρ'/>",
                "<o base='∅' loc='Φ.number.plus.x' name='x'/>",
                "<o atom='Φ.number' loc='Φ.number.plus.λ' name='λ'/></o></o>"
            )
        );
        docs.put(
            "bytes", "<o loc='Φ.bytes' name='bytes'><o base='∅' loc='Φ.bytes.φ' name='φ'/></o>"
        );
        docs.put(
            "foo",
            String.join(
                "",
                "<o loc='Φ.foo' name='foo'>",
                "<o loc='Φ.foo.f' name='f'><o base='∅' loc='Φ.foo.f.a' name='a'/>",
                "<o base='ξ.a.plus' loc='Φ.foo.f.φ' name='φ'><o as='α0' base='Φ.number' loc='Φ.foo.f.φ.α0'>",
                "<o as='α0' base='Φ.bytes' loc='Φ.foo.f.φ.α0.α0'><o as='α0' loc='Φ.foo.f.φ.α0.α0.α0'>",
                "40-14-00-00-00-00-00-00</o></o></o></o></o></o>"
            )
        );
        final List<Path> files = new ArrayList<>(docs.size());
        for (final Map.Entry<String, String> doc : docs.entrySet()) {
            final Path file = temp.resolve(String.format("%s.xmir", doc.getKey()));
            Files.write(
                file,
                String.format("<object>%s</object>", doc.getValue())
                    .getBytes(StandardCharsets.UTF_8)
            );
            files.add(file);
        }
        final Boxes boxes = new Boxes(home.boxes());
        boxes.save(new Planted(files, formas).all());
        Node out = null;
        for (final Map.Entry<String, String> doc : docs.entrySet()) {
            final Node node = new Xnav(temp.resolve(String.format("%s.xmir", doc.getKey())))
                .element("object").node().getOwnerDocument();
            new Xml(new Boxed(node, boxes, "").copy()).saved(home.boxed(doc.getKey()));
            out = node;
        }
        return out;
    }
}
