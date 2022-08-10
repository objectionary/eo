/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

/*
 * @checkstyle PackageNameCheck (4 lines)
 */
package EOorg.EOeolang.EOcollections;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhDefault;
import org.eolang.Phi;

/**
 * Rebuild of multimap.
 * @since 1.0
 * @checkstyle TypeNameCheck (5 lines)
 */
@SuppressWarnings("PMD.AvoidDollarSigns")
public class EOmultimap$EOrebuild extends PhDefault {

    /**
     * Ctor.
     * @param sigma Sigma.
     */
    @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
    public EOmultimap$EOrebuild(final Phi sigma) {
        super(sigma);
        this.add("harr", new AtFree());
        this.add("arr", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final Phi[] harr = new Dataized(rho.attr("harr").get()).take(Phi[].class);
                    final Phi[] arr = new Dataized(rho.attr("arr").get()).take(Phi[].class);
                    final List<Long> hashes = Arrays.stream(harr)
                        .map(item -> new Dataized(item).take(Long.class))
                        .collect(Collectors.toList());
                    final int size = hashes.size();
                    final List<List<Phi>> table = new ArrayList<>(0);
                    IntStream.range(0, size)
                        .forEach(
                            index -> table.add(new ArrayList<>(0)));
                    IntStream.range(0, arr.length)
                        .forEach(
                            index -> table.get((int) (hashes.get(index) % size))
                                .add(arr[index]));
                    final Phi[] result = table.stream()
                        .map(
                            items -> {
                                final Phi[] array = new Phi[items.size()];
                                IntStream.range(0, items.size())
                                    .forEach(index -> array[index] = items.get(index));
                                return new Data.ToPhi(array);
                            }
                        ).toArray(Phi[]::new);
                    return new Data.ToPhi(result);
                }
            )
        );
    }

}
