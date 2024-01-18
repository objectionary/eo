/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

pub enum EO {
    EOVertex(u32),
    EOFloat(f64),
    EOInt(i64),
    EOString(String),
    EORaw(Box<[u8]>),
    EOError(String),
}

impl EO {
    pub fn eo2vec(&self) -> Vec<u8> {
        match self {
            EO::EOVertex(v) => {
                let mut res: Vec<u8> = vec![0; 1 + 4];
                res[0] = 0;
                res[1..].copy_from_slice(&v.to_be_bytes());
                res
            }
            EO::EOFloat(x) => {
                let mut res = vec![0; 1 + 8];
                res[0] = 1;
                res[1..].copy_from_slice(&x.to_be_bytes());
                res
            }
            EO::EOInt(x) => {
                let mut res: Vec<u8> = vec![0; 1 + 8];
                res[0] = 2;
                res[1..].copy_from_slice(&x.to_be_bytes());
                res
            }
            EO::EOString(content) => {
                let content_bytes = content.clone().into_bytes();
                let mut res: Vec<u8> = vec![0; 1 + content_bytes.len()];
                res[0] = 3;
                res[1..].copy_from_slice(&content_bytes);
                res
            }
            EO::EORaw(content) => {
                let mut res: Vec<u8> = vec![0; 1 + content.len()];
                res[0] = 4;
                res[1..].copy_from_slice(&content.to_vec());
                res
            }
            EO::EOError(cause) => {
                let cause_bytes = cause.clone().into_bytes();
                let mut res: Vec<u8> = vec![0; 1 + cause_bytes.len()];
                res[0] = 5;
                res[1..].copy_from_slice(&cause_bytes);
                res
            }
        }
    }
}
