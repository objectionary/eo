/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.w3c.dom.Node;

/**
 * Test case for {@link Boxed}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class BoxedTest {

    @Test
    void plantsLambdaIntoBoxedFormation(@Mktmp final Path temp) throws IOException {
        final Boxes boxes = new Boxes(temp.resolve("boxes.tsv"));
        boxes.save(
            Collections.singletonList(
                new Box(Arrays.asList("L_box_3", "Φ.foo.g", "number", "-", "y:number"))
            )
        );
        MatcherAssert.assertThat(
            "the boxed formation must end with its λ, but it doesnt",
            new Xml(
                new Boxed(
                    BoxedTest.doc(
                        "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.g' name='g'><o base='∅' name='y'/></o></o>"
                    ),
                    boxes, "Φ.foo.f"
                ).copy()
            ).text(),
            Matchers.containsString("<o base=\"∅\" name=\"y\"/><o name=\"λ\">L_box_3</o></o>")
        );
    }

    @Test
    void boxesKeptFormationToo(@Mktmp final Path temp) throws IOException {
        final Boxes boxes = new Boxes(temp.resolve("boxes.tsv"));
        boxes.save(
            Collections.singletonList(
                new Box(Arrays.asList("L_box_3", "Φ.foo.g", "number", "-", "y:number"))
            )
        );
        MatcherAssert.assertThat(
            "the kept formation must carry its box for a recursive entry to hit, but it doesnt",
            new Xml(
                new Boxed(
                    BoxedTest.doc(
                        "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.g' name='g'><o base='∅' name='y'/></o></o>"
                    ),
                    boxes, "Φ.foo.g"
                ).copy()
            ).text(),
            Matchers.containsString("L_box_3")
        );
    }

    @Test
    void trimsProbesAwayFromKeptFormation(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a test the kept formation is not inside must be trimmed, but it wasnt",
            new Xml(
                new Boxed(
                    BoxedTest.doc(
                        String.join(
                            "",
                            "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.p🌵t' name='p🌵t'>",
                            "<o base='Φ.true' name='φ'/></o><o loc='Φ.foo.f' name='f'/></o>"
                        )
                    ),
                    new Boxes(temp.resolve("boxes.tsv")), "Φ.foo.f"
                ).copy()
            ).text(),
            Matchers.not(Matchers.containsString("p🌵t"))
        );
    }

    @Test
    void keepsProbeAroundKeptFormation(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the test the kept formation stands in must stay, but it was trimmed",
            new Xml(
                new Boxed(
                    BoxedTest.doc(
                        String.join(
                            "",
                            "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.n🌵t' name='n🌵t'>",
                            "<o loc='Φ.foo.n🌵t.f' name='f'><o base='∅' name='x'/></o></o></o>"
                        )
                    ),
                    new Boxes(temp.resolve("boxes.tsv")), "Φ.foo.n🌵t.f"
                ).copy()
            ).text(),
            Matchers.containsString("n🌵t")
        );
    }

    @Test
    void leavesOriginalAsItWas(@Mktmp final Path temp) throws IOException {
        final Boxes boxes = new Boxes(temp.resolve("boxes.tsv"));
        boxes.save(
            Collections.singletonList(
                new Box(Arrays.asList("L_box_1", "Φ.foo.g", "number", "-", "y:number"))
            )
        );
        final Node doc = BoxedTest.doc(
            "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.g' name='g'><o base='∅' name='y'/></o></o>"
        );
        new Boxed(doc, boxes, "").copy();
        MatcherAssert.assertThat(
            "the original document must not be touched, but it was",
            new Xml(doc).text(),
            Matchers.not(Matchers.containsString("L_box_1"))
        );
    }

    private static Node doc(final String body) {
        return new Xnav(String.format("<object>%s</object>", body))
            .element("object").node().getOwnerDocument();
    }


    @Test
    void replacesLambdaOfLoweredFormation(@Mktmp final Path temp) throws IOException {
        final Boxes boxes = new Boxes(temp.resolve("boxes.tsv"));
        boxes.save(
            Collections.singletonList(
                new Box(Arrays.asList("L_box_2", "Φ.foo.f", "number", "-", "x:number"))
            )
        );
        MatcherAssert.assertThat(
            "a formation lowered already must carry the box as its only λ, but it doesnt",
            new Xml(
                new Boxed(
                    BoxedTest.doc(
                        String.join(
                            "",
                            "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.f' lowered='ab' name='f'>",
                            "<o base='∅' name='x'/><o atom='Φ.number' name='λ'/></o></o>"
                        )
                    ),
                    boxes, "Φ.foo.g"
                ).copy()
            ).text(),
            Matchers.allOf(
                Matchers.containsString("<o name=\"λ\">L_box_2</o>"),
                Matchers.not(Matchers.containsString("atom="))
            )
        );
    }
}
