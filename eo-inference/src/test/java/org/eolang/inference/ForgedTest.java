/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Forged}.
 * @since 0.71.0
 */
final class ForgedTest {

    @Test
    void marksAnAnswerRootedAtAVoidOnlyAnAtomFills() {
        MatcherAssert.assertThat(
            "a name taken through a void an atom fills must be told apart, but it wasnt",
            new Forged(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides><type id='Φ.syscall' returns='Φ.reply'/>",
                        "<type id='Φ.reply'><attr name='code' type='Φ.reply.code'",
                        " void='true'/></type></provides>"
                    )
                )
            ).marked(
                Collections.singletonMap("Φ.app.size", new Answer("Φ.reply.code.size", 1))
            ).get("Φ.app.size").forged(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void leavesAVoidTheCallersFillAlone() {
        MatcherAssert.assertThat(
            "a void the program fills itself must keep the band it had, but it moved",
            new Forged(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides><type id='Φ.syscall' returns='Φ.reply'/>",
                        "<type id='Φ.reply'><attr name='code' type='Φ.reply.code'",
                        " void='true'/></type></provides>"
                    )
                )
            ).marked(
                Collections.singletonMap(
                    "Φ.app.size",
                    new Answer(
                        "Φ.reply.code.size", 1,
                        Arrays.asList(new Ref("Φ.number"), new Ref("Φ.string"))
                    )
                )
            ).get("Φ.app.size").forged(),
            Matchers.equalTo(false)
        );
    }

    @Test
    void leavesTheReceiverOfSuchAFormationAlone() {
        MatcherAssert.assertThat(
            "whoever dispatches fills the receiver, so it must keep its band, but it moved",
            new Forged(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides><type id='Φ.syscall' returns='Φ.reply'/>",
                        "<type id='Φ.reply'><attr name='ρ' type='Φ.reply.ρ'",
                        " void='true'/></type></provides>"
                    )
                )
            ).marked(
                Collections.singletonMap("Φ.app.name", new Answer("Φ.reply.ρ.name", 1))
            ).get("Φ.app.name").forged(),
            Matchers.equalTo(false)
        );
    }

    @Test
    void leavesTheVoidAnAtomComesBackWithAlone() {
        MatcherAssert.assertThat(
            "an atom that hands back what it was given fills nothing, but its void was marked",
            new Forged(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides><type id='Φ.recovered' returns='Φ.recovered.value'>",
                        "<attr name='value' type='Φ.recovered.value' void='true'/>",
                        "</type></provides>"
                    )
                )
            ).marked(
                Collections.singletonMap("Φ.app.eq", new Answer("Φ.recovered.value.eq", 1))
            ).get("Φ.app.eq").forged(),
            Matchers.equalTo(false)
        );
    }
}
