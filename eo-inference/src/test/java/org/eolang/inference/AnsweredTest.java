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
}
