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
package EOorg.EOeolang;

import java.util.function.BinaryOperator;
import org.eolang.Bytes;
import org.eolang.BytesOf;
import org.eolang.Dataized;
import org.eolang.Expr;
import org.eolang.Param;
import org.eolang.Phi;

/**
 * Reduce on BYTES.
 *
 * @since 1.0
 */
final class ExReduceBytes implements Expr {
    /**
     * Reduce operation.
     */
    private final BinaryOperator<Bytes> oper;

    /**
     * Ctor.
     * @param oper Operation.
     */
    ExReduceBytes(final BinaryOperator<Bytes> oper) {
        this.oper = oper;
    }

    @Override
    public Phi get(final Phi rho) throws Exception {
        Bytes base = new Param(rho).asBytes();
        final Phi[] args = new Param(rho, "b").strong(Phi[].class);
        for (final Phi phi : args) {
            base = this.oper.apply(
                base,
                new BytesOf(
                    new Dataized(phi).take(byte[].class)
                )
            );
        }
        return Bytes.toPhi(base);
    }
}
