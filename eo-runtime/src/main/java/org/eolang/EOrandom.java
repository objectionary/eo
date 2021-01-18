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

package org.eolang;

import java.security.SecureRandom;
import org.eolang.phi.AtBound;
import org.eolang.phi.AtLambda;
import org.eolang.phi.Data;
import org.eolang.phi.PhDefault;
import org.eolang.phi.PhEta;
import org.eolang.phi.Phi;

/**
 * RANDOM.
 *
 * @since 0.1
 */
public class EOrandom extends PhDefault {

    public EOrandom() {
        this(new PhEta());
    }

    public EOrandom(final Phi parent) {
        super(parent);
        this.add("φ", new AtBound(new AtLambda(this, self -> new Data.ToPhi(
            new SecureRandom().nextDouble()
        ))));
    }

}
