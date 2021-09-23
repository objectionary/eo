/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

package EOorg.EOeolang;

import java.util.concurrent.atomic.AtomicReference;
import org.eolang.phi.AtBound;
import org.eolang.phi.AtFree;
import org.eolang.phi.AtLambda;
import org.eolang.phi.Data;
import org.eolang.phi.Dataized;
import org.eolang.phi.PhDefault;
import org.eolang.phi.PhEta;
import org.eolang.phi.Phi;

/**
 * MEMORY.
 *
 * @since 1.0
 */
public class EOmemory extends PhDefault {

    private final AtomicReference<Phi> phi = new AtomicReference<>(new PhEta());

    public EOmemory() {
        this(new PhEta());
    }

    public EOmemory(final Phi parent) {
        super(parent);
        this.add("φ", new AtBound(new AtLambda(this, self -> this.phi.get())));
        this.add("write", new AtBound(new AtLambda(this, EOmemory.Write::new)));
    }

    @Override
    public final String toString() {
        return String.format("<%s>", this.phi.get());
    }

    private final class Write extends PhDefault {
        Write(final Phi parent) {
            super(parent);
            this.add("x", new AtFree());
            this.add("φ", new AtBound(new AtLambda(this, self -> {
                final Object obj = new Dataized(self.attr("x").get()).take();
                EOmemory.this.phi.set(new Data.ToPhi(obj));
                return new Data.ToPhi(true);
            })));
        }
    }

}
