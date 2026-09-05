/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Demands}.
 * @since 0.69.0
 */
final class DemandsTest {

    @Test
    void gathersWhatIsAskedOfAnAnswerOntoTheVoidItCameFrom() {
        final Map<String, Map<String, String>> asked = new LinkedHashMap<>(0);
        asked.put("Φ.inc.x", Collections.singletonMap("next", "Φ.inc.x.next"));
        asked.put("Φ.inc.x.next", Collections.singletonMap("foo", "Φ.inc.x.next.foo"));
        MatcherAssert.assertThat(
            "the name asked of the answer must stand beside the one that named it, but it didnt",
            new XMLDocument(
                new Xembler(
                    new Directives().add("attr").append(
                        new Demands(
                            asked, new Rooted(Collections.singletonList("Φ.inc.x"))
                        ).directives()
                    )
                ).xmlQuietly()
            ).nodes("/attr/demand[@of='Φ.inc.x.next' and @name='foo']"),
            Matchers.hasSize(1)
        );
    }

    @Test
    void leavesAloneWhatIsAskedOfAnotherVoid() {
        MatcherAssert.assertThat(
            "a name asked of another void must stay there, but it came here",
            new Demands(
                Collections.singletonMap(
                    "Φ.dec.y", Collections.singletonMap("prev", "Φ.dec.y.prev")
                ),
                new Rooted(Collections.singletonList("Φ.inc.x"))
            ).any(),
            Matchers.is(false)
        );
    }
}
