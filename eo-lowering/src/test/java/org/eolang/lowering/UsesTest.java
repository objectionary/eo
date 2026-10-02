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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.cactoos.Scalar;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests of the class {@link Uses}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class UsesTest {

    @Test
    void givesEveryThreadTheHashItGetsAlone(@Mktmp final Path temp) throws IOException {
        final int count = 300;
        final Collection<String> rows = new ArrayList<>(count);
        final Collection<Integer> numbers = new ArrayList<>(count);
        final StringBuilder body = new StringBuilder(0);
        for (int idx = 0; idx < 20; ++idx) {
            body.append(String.format("<o name='k%d' base='Φ.x'><o base='Φ.y'/></o>", idx));
        }
        for (int num = 1; num <= count; ++num) {
            rows.add(String.format("<o name='e%d'><o base='Φ.e%d'/>%s</o>", num, num, body));
            numbers.add(num);
        }
        Files.write(
            temp.resolve("entries.xmir"),
            String.format(
                "<object><o><o name='mark'/><o name='root'/>%s</o></object>",
                String.join("", rows)
            ).getBytes(StandardCharsets.UTF_8)
        );
        final Uses single = new Uses(temp);
        final List<String> alone = new ArrayList<>(count);
        for (final int num : numbers) {
            alone.add(single.hash(num, String.format("Φ.e%d", num)));
        }
        final Uses shared = new Uses(temp);
        for (int round = 0; round < 20; ++round) {
            MatcherAssert.assertThat(
                "every thread must get the hash its entry gets alone, but some didnt",
                new ListOf<>(
                    new Threads<>(
                        32,
                        new Mapped<Scalar<String>>(
                            num -> () -> shared.hash(num, String.format("Φ.e%d", num)),
                            numbers
                        )
                    )
                ),
                Matchers.equalTo(alone)
            );
        }
    }
}
