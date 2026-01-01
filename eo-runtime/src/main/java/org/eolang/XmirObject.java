/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation for a Java class made from XMIR object.
 *
 * @since 0.17
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface XmirObject {

    /**
     * The original name of the object in EO, before optimization.
     *
     * @return The name as it was in EO
     */
    String oname();

    /**
     * The name of the object in EO.
     *
     * @return The name as it is in EO
     */
    String name() default "";

    /**
     * The name of the source file where this Java code was generated from.
     *
     * @return The absolute path
     */
    String source() default "";

}
