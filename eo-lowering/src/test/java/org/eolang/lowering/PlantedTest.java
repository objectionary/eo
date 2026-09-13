/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Planted}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class PlantedTest {

    @Test
    void boxesNamedFormationWithVoids(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a named formation with a void must be boxed with the forma of it, but it wasnt",
            new Planted(
                Collections.singletonList(
                    PlantedTest.doc(
                        temp,
                        String.join(
                            "",
                            "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.f' name='f'>",
                            "<o base='∅' name='x'/><o base='ξ.x' name='φ'/></o></o>"
                        )
                    )
                ),
                new Formas(Collections.emptyMap(), Collections.singletonMap("Φ.foo.f.x", "number"))
            ).all().get(0).line(),
            Matchers.equalTo("L_box_1\tΦ.foo.f\tobject\t-\tx:number")
        );
    }

    @Test
    void leavesThunkUnboxed(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a formation without voids is no box, but it was boxed",
            new Planted(
                Collections.singletonList(
                    PlantedTest.doc(
                        temp,
                        "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.f' name='f'><o base='Φ.true' name='φ'/></o></o>"
                    )
                ),
                new Formas(Collections.emptyMap(), Collections.emptyMap())
            ).all(),
            Matchers.empty()
        );
    }

    @Test
    void leavesFormationUnderAnonymousOneUnboxed(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a named formation inside an anonymous one is no box, but it was boxed",
            new Planted(
                Collections.singletonList(
                    PlantedTest.doc(
                        temp,
                        String.join(
                            "",
                            "<o loc='Φ.foo' name='foo'><o base='Φ.g' name='φ'><o as='α0'>",
                            "<o loc='Φ.foo.φ.α0.h' name='h'><o base='∅' name='z'/></o>",
                            "</o></o></o>"
                        )
                    )
                ),
                new Formas(Collections.emptyMap(), Collections.emptyMap())
            ).all(),
            Matchers.empty()
        );
    }

    @Test
    void leavesAtomUnboxed(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a formation with a λ of its own is no box, but it was boxed",
            new Planted(
                Collections.singletonList(
                    PlantedTest.doc(
                        temp,
                        String.join(
                            "",
                            "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.f' name='f'>",
                            "<o base='∅' name='x'/><o atom='Φ.number' name='λ'/></o></o>"
                        )
                    )
                ),
                new Formas(Collections.emptyMap(), Collections.emptyMap())
            ).all(),
            Matchers.empty()
        );
    }

    @Test
    void leavesDataObjectUnboxed(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a data object is the carrier of its markers and no box, but it was boxed",
            new Planted(
                Collections.singletonList(
                    PlantedTest.doc(
                        temp,
                        "<o loc='Φ.number' name='number'><o base='∅' name='φ'/><o base='ξ.φ' name='as-bytes'/></o>"
                    )
                ),
                new Formas(Collections.emptyMap(), Collections.emptyMap())
            ).all(),
            Matchers.empty()
        );
    }

    @Test
    void recordsFormaOfReceiverWhenBodyReaches(@Mktmp final Path temp) throws IOException {
        final Map<String, String> given = new HashMap<>(2);
        given.put("Φ.foo.f.ρ", "bool");
        given.put("Φ.foo.f.x", "number");
        MatcherAssert.assertThat(
            "the forma of ρ must be recorded when the body reaches for it, but it wasnt",
            new Planted(
                Collections.singletonList(
                    PlantedTest.doc(
                        temp,
                        String.join(
                            "",
                            "<o loc='Φ.foo' name='foo'><o loc='Φ.foo.f' name='f'>",
                            "<o base='∅' name='x'/><o base='ξ.ρ.y' name='φ'/></o></o>"
                        )
                    )
                ),
                new Formas(Collections.emptyMap(), given)
            ).all().get(0).parent(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void numbersBoxesAcrossDocuments(@Mktmp final Path temp) throws IOException {
        final String body = "<o base='∅' name='x'/><o base='ξ.x' name='φ'/>";
        MatcherAssert.assertThat(
            "the boxes of all documents must be numbered in one sequence, but they arent",
            new Planted(
                Arrays.asList(
                    PlantedTest.doc(
                        temp,
                        String.format("<o loc='Φ.a' name='a'><o loc='Φ.a.f' name='f'>%s</o></o>", body)
                    ),
                    PlantedTest.doc(
                        temp,
                        String.format("<o loc='Φ.b' name='b'><o loc='Φ.b.g' name='g'>%s</o></o>", body)
                    )
                ),
                new Formas(Collections.emptyMap(), Collections.emptyMap())
            ).all().stream().map(Box::lambda).collect(Collectors.toList()),
            Matchers.contains("L_box_1", "L_box_2")
        );
    }

    private static Path doc(final Path temp, final String body) throws IOException {
        final Path out = Files.createTempFile(temp, "doc", ".xmir");
        Files.write(
            out, String.format("<object>%s</object>", body).getBytes(StandardCharsets.UTF_8)
        );
        return out;
    }
}
