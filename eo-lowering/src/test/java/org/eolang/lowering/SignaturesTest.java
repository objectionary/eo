/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests of the class {@link Signatures}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class SignaturesTest {

    @Test
    void printsTheTypesOfTheVoidsAndOfTheBody(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("facts.xml"),
            String.join(
                "",
                "<facts><objects><type id='Φ.kx.w🌵q'>",
                "<attr name='ρ' rho='Φ.kx' type='Φ.kx.w🌵q.ρ' void='true'/>",
                "<attr name='zz' type='Φ.kx.w🌵q.zz' void='true'/>",
                "<attr name='φ' type='Φ.kx.w🌵q.φ'/>",
                "</type></objects></facts>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        Files.write(
            temp.resolve("answers.xml"),
            String.join(
                "",
                "<answers>",
                "<o loc='Φ.kx.w🌵q.zz' copies='3'><is loc='Φ.string'/></o>",
                "<o loc='Φ.kx.w🌵q.φ' copies='3'><is loc='Φ.i32'/></o>",
                "</answers>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "the signature must show the types that go in and the type that comes out, but it doesnt",
            new Signatures(temp).of("Φ.kx.w🌵q"),
            Matchers.equalTo("w🌵q(Φ.kx, Φ.string)→ Φ.i32")
        );
    }

    @Test
    void takesTheTypeOfTheAtomTheBodyIs() {
        MatcherAssert.assertThat(
            "the body that is an atom must come out as the type that atom returns, but it doesnt",
            new Signatures(
                () -> new XMLDocument(
                    "<facts><objects><type id='Φ.number.times' returns='Φ.number'/></objects></facts>"
                ),
                () -> new XMLDocument(
                    "<answers><o loc='Φ.oj.φ' copies='5'><is loc='Φ.number.times'/></o></answers>"
                )
            ).of("Φ.oj"),
            Matchers.endsWith("→ Φ.number")
        );
    }

    @Test
    void takesWhatTheFormationTheBodyIsReducesTo() {
        MatcherAssert.assertThat(
            "the body that is a decorator must come out as what that decorator reduces to, but it doesnt",
            new Signatures(
                () -> new XMLDocument("<facts/>"),
                () -> new XMLDocument(
                    String.join(
                        "",
                        "<answers>",
                        "<o loc='Φ.pq' copies='2'><is loc='Φ.pq'/>",
                        "<reduced copies='2'><set copies='1'><is loc='Φ.pq'/></set>",
                        "<set copies='1'><terminator/><is loc='Φ.bool'/></set></reduced></o>",
                        "<o loc='Φ.ur.φ' copies='2'><is loc='Φ.pq'/></o>",
                        "</answers>"
                    )
                )
            ).of("Φ.ur"),
            Matchers.equalTo("ur()→ Φ.bool")
        );
    }

    @Test
    void leavesTheTerminationsAndTheFailingReadsOutOfTheBody() {
        MatcherAssert.assertThat(
            "the type of the body must leave a termination and a failing read out, but it doesnt",
            new Signatures(
                () -> new XMLDocument("<facts/>"),
                () -> new XMLDocument(
                    String.join(
                        "",
                        "<answers><o loc='Φ.zv.φ' copies='9'>",
                        "<set copies='6'><terminator/><is loc='Φ.bytes'/></set>",
                        "<set copies='3'><fails name='Φ.zv.size'/><is loc='Φ.bytes'/></set>",
                        "</o></answers>"
                    )
                )
            ).of("Φ.zv"),
            Matchers.equalTo("zv()→ Φ.bytes")
        );
    }

    @Test
    void takesTheOneObjectAVoidHoldsBesideTheTerminations() {
        MatcherAssert.assertThat(
            "a void must show the one object its copies hold, with a termination left out, but it doesnt",
            new Signatures(
                () -> new XMLDocument(
                    "<facts><objects><type id='Φ.hb'><attr name='k' void='true'/></type></objects></facts>"
                ),
                () -> new XMLDocument(
                    String.join(
                        "",
                        "<answers><o loc='Φ.hb.k' copies='7'>",
                        "<set copies='4'><terminator/><is loc='Φ.bool'/></set>",
                        "<set copies='3'><is loc='Φ.bool'/></set>",
                        "</o></answers>"
                    )
                )
            ).of("Φ.hb"),
            Matchers.startsWith("hb(Φ.bool)")
        );
    }

    @Test
    void marksAVoidThatHoldsTwoObjects() {
        MatcherAssert.assertThat(
            "a void whose copies hold two objects must be shown as a question mark, but it isnt",
            new Signatures(
                () -> new XMLDocument(
                    "<facts><objects><type id='Φ.mw'><attr name='e' void='true'/></type></objects></facts>"
                ),
                () -> new XMLDocument(
                    String.join(
                        "",
                        "<answers><o loc='Φ.mw.e' copies='2'>",
                        "<set copies='1'><is loc='Φ.number'/></set>",
                        "<set copies='1'><is loc='Φ.string'/></set>",
                        "</o></answers>"
                    )
                )
            ).of("Φ.mw"),
            Matchers.startsWith("mw(?)")
        );
    }

    @Test
    void marksAVoidThatHoldsADatumBesideAnObject() {
        MatcherAssert.assertThat(
            "a void that holds a datum beside an object must be shown as a question mark, but it isnt",
            new Signatures(
                () -> new XMLDocument(
                    "<facts><objects><type id='Φ.tc'><attr name='j' void='true'/></type></objects></facts>"
                ),
                () -> new XMLDocument(
                    String.join(
                        "",
                        "<answers><o loc='Φ.tc.j' copies='2'>",
                        "<set copies='1'><data/></set>",
                        "<set copies='1'><is loc='Φ.bytes'/></set>",
                        "</o></answers>"
                    )
                )
            ).of("Φ.tc"),
            Matchers.startsWith("tc(?)")
        );
    }

    @Test
    void readsTheReceiverOffTheFormationItIsWrittenIn() {
        MatcherAssert.assertThat(
            "a declared receiver must hold the formation it is written in, but it doesnt",
            new Signatures(
                () -> new XMLDocument(
                    "<facts><objects><type id='Φ.gk.dd'><attr name='ρ' rho='Φ.gk' void='true'/></type></objects></facts>"
                ),
                () -> new XMLDocument(
                    "<answers><o loc='Φ.gk.dd.ρ' copies='4'><is loc='Φ.number'/></o></answers>"
                )
            ).of("Φ.gk.dd"),
            Matchers.startsWith("dd(Φ.gk)")
        );
    }

    @Test
    void dropsTheMarkOfAVoidThatMayBeBottom() {
        MatcherAssert.assertThat(
            "the type of a void must be shown without its mark of a maybe-⊥, but it isnt",
            new Signatures(
                () -> new XMLDocument(
                    "<facts><objects><type id='Φ.rd'><attr name='y' void='true' holds='Φ.bytes?'/></type></objects></facts>"
                ),
                () -> new XMLDocument("<answers/>")
            ).of("Φ.rd"),
            Matchers.startsWith("rd(Φ.bytes)")
        );
    }

    @Test
    void marksATypeTheTablesDontKnow(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("facts.xml"),
            "<facts><objects><type id='Φ.ve'><attr name='g' void='true'/></type></objects></facts>"
                .getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "a type the tables dont know must be shown as a question mark, but it isnt",
            new Signatures(temp).of("Φ.ve"),
            Matchers.equalTo("ve(?)→ ?")
        );
    }
}
