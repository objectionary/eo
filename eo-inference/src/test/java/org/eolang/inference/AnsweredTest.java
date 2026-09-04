/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Answered}.
 * @since 0.71.0
 */
@ExtendWith(MktmpResolver.class)
final class AnsweredTest {

    @Test
    void answersTheBodyOfAnAtomWithTheFormaItDeclares(@Mktmp final Path temp)
        throws IOException {
        final Path world = Files.createDirectories(temp.resolve("xmirs"));
        Files.writeString(
            world.resolve("cup.xmir"),
            String.join(
                "",
                "<object><o line='1' loc='Φ.cup' name='cup' pos='0'>",
                "<o line='2' loc='Φ.cup.pour' name='pour' pos='2'>",
                "<o atom='Φ.mug' line='2' loc='Φ.cup.pour.λ' name='λ' pos='2'/>",
                "</o></o><o line='3' loc='Φ.mug' name='mug' pos='0'/></object>"
            )
        );
        final Path tables = temp.resolve("tables");
        new Resolved(new Clues()).follow(world, tables);
        MatcherAssert.assertThat(
            "the body of an atom must answer with the forma the atom declares, but it didnt",
            new Answered(world, tables).all().get("Φ.cup.pour.λ").where(),
            Matchers.equalTo("Φ.mug")
        );
    }

    @Test
    void answersTheBodyOfAnAtomWithTheVoidItComesBackWith(@Mktmp final Path temp)
        throws IOException {
        final Path world = Files.createDirectories(temp.resolve("xmirs"));
        Files.writeString(
            world.resolve("pick.xmir"),
            String.join(
                "",
                "<object><o line='1' loc='Φ.pick' name='pick' pos='0'>",
                "<o base='∅' line='2' loc='Φ.pick.left' name='left' pos='2' type='A?'/>",
                "<o base='∅' line='3' loc='Φ.pick.right' name='right' pos='2' type='A'/>",
                "<o atom='A' line='1' loc='Φ.pick.λ' name='λ' pos='0'/></o></object>"
            )
        );
        final Path tables = temp.resolve("tables");
        new Resolved(new Clues()).follow(world, tables);
        MatcherAssert.assertThat(
            "an atom that comes back with a variable must answer with its void, but it didnt",
            new Answered(world, tables).all().get("Φ.pick.λ").where(),
            Matchers.equalTo("Φ.pick.left")
        );
    }

    @Test
    void tellsApartTheVoidsOnlyAnAtomFills(@Mktmp final Path temp)
        throws IOException {
        final Path world = Files.createDirectories(temp.resolve("xmirs"));
        Files.writeString(
            world.resolve("reply.xmir"),
            String.join(
                "",
                "<object><o line='1' loc='Φ.reply' name='reply' pos='0'>",
                "<o base='∅' line='2' loc='Φ.reply.code' name='code' pos='2'/></o>",
                "<o line='3' loc='Φ.syscall' name='syscall' pos='0'>",
                "<o atom='Φ.reply' line='3' loc='Φ.syscall.λ' name='λ' pos='0'/>",
                "</o></object>"
            )
        );
        final Path tables = temp.resolve("tables");
        new Resolved(new Clues()).follow(world, tables);
        MatcherAssert.assertThat(
            "a void nothing but an atom fills must be told apart, but it wasnt",
            new Answered(world, tables).all().get("Φ.reply.code").forged(),
            Matchers.equalTo(true)
        );
    }

    @Test
    void answersACallerOfSuchAnAtomWithWhatItPutIn(@Mktmp final Path temp)
        throws IOException {
        final Path world = Files.createDirectories(temp.resolve("xmirs"));
        Files.writeString(
            world.resolve("cup.xmir"),
            String.join(
                "",
                "<object><o line='1' loc='Φ.pick' name='pick' pos='0'>",
                "<o base='∅' line='2' loc='Φ.pick.left' name='left' pos='2' type='A?'/>",
                "<o base='∅' line='3' loc='Φ.pick.right' name='right' pos='2' type='A'/>",
                "<o atom='A' line='1' loc='Φ.pick.λ' name='λ' pos='0'/></o>",
                "<o line='4' loc='Φ.mug' name='mug' pos='0'>",
                "<o line='5' loc='Φ.mug.hot' name='hot' pos='2'/></o>",
                "<o line='6' loc='Φ.cup' name='cup' pos='0'>",
                "<o base='.hot' line='7' loc='Φ.cup.φ' name='φ' pos='2'>",
                "<o base='Φ.pick' line='7' loc='Φ.cup.φ.ρ' pos='2'>",
                "<o as='α0' base='Φ.mug' line='7' loc='Φ.cup.φ.ρ.α0' pos='2'/>",
                "<o as='α1' base='Φ.mug' line='7' loc='Φ.cup.φ.ρ.α1' pos='2'/>",
                "</o></o></o></object>"
            )
        );
        final Path tables = temp.resolve("tables");
        new Resolved(new Clues()).follow(world, tables);
        MatcherAssert.assertThat(
            "a name taken from such an atom must answer with what the caller put in, but it didnt",
            new Answered(world, tables).all().get("Φ.cup.φ").where(),
            Matchers.equalTo("Φ.mug.hot")
        );
    }
}
