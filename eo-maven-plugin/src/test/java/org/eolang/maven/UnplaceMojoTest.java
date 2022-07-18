/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.yegor256.tojos.Csv;
import com.yegor256.tojos.MonoTojos;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link UnplaceMojo}.
 *
 * @since 0.1
 */
public final class UnplaceMojoTest {

    @Test
    public void testCleaning(@TempDir final Path temp) throws Exception {
        // final Path dir = temp.resolve("aa/bb/cb/dir");  // it does not create folder
        final Path foo = temp.resolve("a/b/c/foo.class");
        new File(temp + "/a/b/c/dir").mkdirs();
        new File(temp + "/aa/b/c/d/e/dir").mkdirs();
        new File(temp + "/aaa/b/c/dir/subdir").mkdirs();

        new Save("abc", foo).save();
        final Path list = temp.resolve("placed.json");
<<<<<<< HEAD
        new MonoTojos(new Csv(list)).add(foo.toString())
            .set("kind", "class")
            .set(PlaceMojo.ATTR_RELATED, "---")
            .set("hash", new FileHash(foo));
=======
        new MonoTojos(new Csv(list)).add(foo.toString());
        new MonoTojos(new Csv(list)).add(temp + "/a/b/c/dir");
        new MonoTojos(new Csv(list)).add(temp + "/aa/b/c/d/e/dir");
        new MonoTojos(new Csv(list)).add(temp + "/aaa/b/c/dir/subdir");
>>>>>>> 2c3f253f52e6fe64245a056e6c1671426bdbb275
        new Moja<>(UnplaceMojo.class)
            .with("placed", list.toFile())
            .with("placedFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            Files.exists(foo),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            Files.exists(Paths.get(temp + "/a/b/c/dir")),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            Files.exists(Paths.get(temp + "/aa/b/c/d/e/dir")),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            Files.exists(Paths.get(temp + "/aaa/b/c/dir")),  // It fails (should it?)
            Matchers.is(false)
        );
    }

}
