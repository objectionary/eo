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
/**
 * EO-maven-plugin Mojos package.
 *  - The {@link org.eolang.maven.mojos.SafeMojo}: abstract Mojo for all others.
 *  - The {@link org.eolang.maven.mojos.AssembleMojo} includes the following:
 *     - The {@link org.eolang.maven.mojos.ParseMojo}: parse '.eo' -> '.xmir'
 *     - The {@link org.eolang.maven.mojos.OptimizeMojo}: apply XSL-s to '.xmir'
 *     - The {@link org.eolang.maven.mojos.DiscoverMojo}: find all foreign objects
 *     - The {@link org.eolang.maven.mojos.PullMojo}: download foreign '.eo' objects
 *     - The {@link org.eolang.maven.mojos.ResolveMojo}: download and unpack '.jar' artifacts
 *     - The {@link org.eolang.maven.mojos.MarkMojo}: mark '.eo' sources found in '.jar' as foreign
 *     - The {@link org.eolang.maven.mojos.PlaceMojo}: move artifact '.class' files
 *       to 'target/classes/'
 *  - The {@link org.eolang.maven.mojos.TranspileMojo}: '.xmir' -> '.java'
 *  - The {@link org.eolang.maven.mojos.UnplaceMojo}: remove artifact '.class' files
 *  - The {@link org.eolang.maven.mojos.UnspileMojo}: remove auto-generated '.java' files
 *  - The {@link org.eolang.maven.mojos.CopyMojo}: copy '.eo' files to 'EO-SOURCES/' inside '.jar'
 *  - The {@link org.eolang.maven.mojos.RegisterMojo}: register all '.eo' sources
 *    in the 'foreign' catalog
 *  - The {@link org.eolang.maven.mojos.BinarizeMojo}: compile Rust binaries
 *    and put them to `target/` (NYI)
 *  - The {@link org.eolang.maven.mojos.DemandMojo}: add object names to the 'foreign' registry
 *    as demanded
 *  - The {@link org.eolang.maven.mojos.SodgMojo}: Convert 'xmir' to 'sodg'
 *  - The {@link org.eolang.maven.mojos.Moja}: mutable mojo builder
 *  - The {@link org.eolang.maven.mojos.CleanMojo}: delete 'target/eo' directory
 */
package org.eolang.maven.mojos;
