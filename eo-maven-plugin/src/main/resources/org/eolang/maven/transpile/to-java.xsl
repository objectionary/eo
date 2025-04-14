<?xml version="1.0" encoding="UTF-8"?>
<!--
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" id="to-java" version="2.0">
  <xsl:import href="/org/eolang/parser/_datas.xsl"/>
  <xsl:import href="/org/eolang/parser/_specials.xsl"/>
  <xsl:param name="disclaimer" select="'This file was auto-generated by eo-maven-plugin, your changes will be discarded on the next build'"/>
  <xsl:output encoding="UTF-8" method="xml"/>
  <!-- VARIABLES -->
  <xsl:variable name="TAB">
    <xsl:value-of select="$eo:space"/>
    <xsl:value-of select="$eo:space"/>
  </xsl:variable>
  <!-- FUNCTIONS -->
  <!-- EOL -->
  <xsl:function name="eo:eol">
    <xsl:param name="tabs"/>
    <xsl:value-of select="$eo:new-line"/>
    <xsl:value-of select="eo:tabs($tabs)"/>
  </xsl:function>
  <!-- TABS -->
  <xsl:function name="eo:tabs">
    <xsl:param name="n"/>
    <xsl:for-each select="1 to $n">
      <xsl:value-of select="$TAB"/>
    </xsl:for-each>
  </xsl:function>
  <!-- Get clean escaped object name  -->
  <xsl:function name="eo:clean" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:value-of select="concat('EO', replace(replace(translate(translate(replace($n, '_', '__'), '-', '_'), '@', $eo:phi), $eo:alpha, '_'), '\$', '\$EO'))"/>
  </xsl:function>
  <!-- Get object name with suffix -->
  <xsl:function name="eo:suffix" as="xs:string">
    <xsl:param name="s1"/>
    <xsl:param name="s2"/>
    <xsl:value-of select="concat(concat($s1, '_'), $s2)"/>
  </xsl:function>
  <!-- Get class name for the object -->
  <xsl:function name="eo:class-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:param name="alt" as="xs:string"/>
    <xsl:variable name="parts" select="tokenize($n, '\.')"/>
    <xsl:variable name="p">
      <xsl:for-each select="$parts">
        <xsl:if test="position()!=last()">
          <xsl:value-of select="eo:clean(.)"/>
          <xsl:text>.</xsl:text>
        </xsl:if>
      </xsl:for-each>
    </xsl:variable>
    <xsl:variable name="c">
      <xsl:choose>
        <xsl:when test="$parts[last()]">
          <xsl:value-of select="$parts[last()]"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$parts"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="pre" select="concat($p, eo:clean($c))"/>
    <xsl:choose>
      <xsl:when test="string-length($pre)&gt;250">
        <xsl:value-of select="concat(substring($pre, 1, 25), $alt)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$pre"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Get name for special attributes  -->
  <xsl:function name="eo:attr-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:param name="wrap" as="xs:boolean"/>
    <xsl:variable name="alpha" select="starts-with($n, $eo:alpha)"/>
    <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="$n='@'">
          <xsl:value-of select="$eo:phi"/>
        </xsl:when>
        <xsl:when test="$alpha">
          <xsl:value-of select="substring-after($n, $eo:alpha)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$n"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$wrap">
        <xsl:choose>
          <xsl:when test="$alpha">
            <xsl:value-of select="$name"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:variable name="quoted">
              <xsl:text>"</xsl:text>
              <xsl:value-of select="$name"/>
              <xsl:text>"</xsl:text>
            </xsl:variable>
            <xsl:value-of select="$quoted"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Convert location to class name  -->
  <xsl:function name="eo:loc-to-class">
    <xsl:param name="loc"/>
    <xsl:value-of select="concat('EO', replace(translate(string-join(tokenize($loc, '\.'), ''), '-', '_'), $eo:cactoos, $eo:alpha))"/>
  </xsl:function>
  <!-- Get RHO variable depends on context -->
  <xsl:function name="eo:rho">
    <xsl:param name="context"/>
    <xsl:if test="$context!='this'">
      <xsl:value-of select="$context"/>
    </xsl:if>
    <xsl:text>h</xsl:text>
  </xsl:function>
  <!-- Get current context variable depends on provided previous context -->
  <xsl:function name="eo:context">
    <xsl:param name="context"/>
    <xsl:if test="$context!='this'">
      <xsl:value-of select="$context"/>
    </xsl:if>
    <xsl:text>r</xsl:text>
  </xsl:function>
  <!-- Print first FQN character -->
  <xsl:function name="eo:fqn-start">
    <xsl:param name="first"/>
    <xsl:param name="rho"/>
    <xsl:choose>
      <xsl:when test="$first='Q'">
        <xsl:text>Phi.Φ</xsl:text>
      </xsl:when>
      <xsl:when test="$first='$'">
        <xsl:value-of select="$rho"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">
          <xsl:text>FQN must start with either with Q or $, but </xsl:text>
          <xsl:value-of select="$first"/>
          <xsl:text> found</xsl:text>
        </xsl:message>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <!-- Take method -->
  <xsl:function name="eo:method">
    <xsl:param name="base"/>
    <xsl:param name="mtd"/>
    <xsl:choose>
      <xsl:when test="$mtd='^'">
        <xsl:value-of select="$base"/>
        <xsl:text>.take("</xsl:text>
        <xsl:value-of select="$eo:rho"/>
        <xsl:text>");</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>new PhMethod(</xsl:text>
        <xsl:value-of select="$base"/>
        <xsl:text>, </xsl:text>
        <xsl:value-of select="eo:attr-name($mtd, true())"/>
        <xsl:text>);</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:variable name="object-name">
    <xsl:variable name="pckg" select="/object/class/@package"/>
    <xsl:variable name="obj" select="/object/class/@name"/>
    <xsl:choose>
      <xsl:when test="$pckg">
        <xsl:value-of select="$pckg"/>
        <xsl:text>.</xsl:text>
        <xsl:value-of select="$obj"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$obj"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <!-- Class. Entry point  -->
  <xsl:template match="class">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="java">
        <xsl:apply-templates select="/object" mode="license"/>
        <xsl:apply-templates select="/object/metas/meta[head='package']" mode="head"/>
        <xsl:text>import java.util.function.Function;</xsl:text>
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:text>import org.eolang.*;</xsl:text>
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:apply-templates select="/object/metas/meta[head='tests']" mode="head"/>
        <xsl:apply-templates select="." mode="body"/>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <!-- Class name  -->
  <xsl:template match="class/@name">
    <xsl:attribute name="name">
      <xsl:value-of select="."/>
    </xsl:attribute>
    <xsl:attribute name="java-name">
      <xsl:variable name="pkg" select="/object/metas/meta[head='package']/part[1]"/>
      <xsl:if test="$pkg">
        <xsl:value-of select="eo:class-name($pkg, eo:suffix(../@line, ../@pos))"/>
        <xsl:text>.</xsl:text>
      </xsl:if>
      <xsl:value-of select="eo:class-name(., eo:suffix(../@line, ../@pos))"/>
    </xsl:attribute>
  </xsl:template>
  <!-- Class body  -->
  <xsl:template match="class" mode="body">
    <xsl:apply-templates select="xmir"/>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text>@XmirObject(name = "</xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>", oname = "</xsl:text>
    <xsl:choose>
      <xsl:when test="@original-name">
        <xsl:value-of select="@original-name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@name"/>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>", source = "</xsl:text>
    <xsl:value-of select="replace(/object/@source, '\\', '\\\\')"/>
    <xsl:text>")</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text>public final class </xsl:text>
    <xsl:value-of select="eo:class-name(@name, eo:suffix(@line, @pos))"/>
    <xsl:choose>
      <xsl:when test="@base">
        <xsl:text> extends PhOnce {</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text> extends PhDefault {</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:apply-templates select="." mode="ctors"/>
    <xsl:if test="/object/metas/meta[head='tests']">
      <xsl:apply-templates select="." mode="tests"/>
    </xsl:if>
    <xsl:apply-templates select="nested"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <!-- Nested classes for anonymous abstract objects  -->
  <xsl:template match="nested">
    <xsl:variable name="name" select="eo:loc-to-class(@loc)"/>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text>private static class </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> extends PhDefault {</xsl:text>
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:text>/**</xsl:text>
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:text> * Ctor.</xsl:text>
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:text> */</xsl:text>
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:value-of select="$name"/>
    <xsl:text>() {</xsl:text>
    <xsl:apply-templates select="attr">
      <xsl:with-param name="indent" select="3"/>
      <xsl:with-param name="context" select="'this'"/>
      <xsl:with-param name="parent" select="$name"/>
    </xsl:apply-templates>
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <!-- Xmir comment  -->
  <xsl:template match="xmir">
    <xsl:text>/**</xsl:text>
    <xsl:for-each select="tokenize(text(), '&#10;')">
      <xsl:value-of select="eo:eol(0)"/>
      <xsl:text> * </xsl:text>
      <xsl:value-of select="."/>
    </xsl:for-each>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text> */</xsl:text>
  </xsl:template>
  <!-- Class constructor -->
  <xsl:template match="class" mode="ctors">
    <xsl:variable name="class" select="eo:class-name(@name, eo:suffix(@line, @pos))"/>
    <xsl:text>/**</xsl:text>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text> * Ctor.</xsl:text>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text> */</xsl:text>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text>public </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>() {</xsl:text>
    <xsl:choose>
      <xsl:when test="@base">
        <xsl:value-of select="eo:eol(2)"/>
        <xsl:text>super(</xsl:text>
        <xsl:value-of select="eo:eol(3)"/>
        <xsl:text>() -&gt; {</xsl:text>
        <xsl:apply-templates select="o" mode="object">
          <xsl:with-param name="name" select="'r'"/>
          <xsl:with-param name="indent" select="4"/>
        </xsl:apply-templates>
        <xsl:value-of select="eo:eol(4)"/>
        <xsl:text>return r;</xsl:text>
        <xsl:value-of select="eo:eol(3)"/>
        <xsl:text>}</xsl:text>
        <xsl:value-of select="eo:eol(2)"/>
        <xsl:text>);</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="attr">
          <xsl:with-param name="indent" select="2"/>
          <xsl:with-param name="parent" select="$class"/>
          <xsl:with-param name="context" select="'this'"/>
        </xsl:apply-templates>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <!-- Attribute -->
  <xsl:template match="attr">
    <xsl:param name="indent"/>
    <xsl:param name="parent"/>
    <xsl:param name="context"/>
    <xsl:variable name="name" select="eo:attr-name(@name, false())"/>
    <xsl:if test="not(@name)">
      <xsl:message terminate="yes">
        <xsl:text>Unnamed attribute found in </xsl:text>
        <xsl:value-of select="parent::*/@loc"/>
      </xsl:message>
    </xsl:if>
    <xsl:value-of select="eo:eol($indent)"/>
    <xsl:if test="$context!='this'">
      <xsl:text>((PhDefault) </xsl:text>
    </xsl:if>
    <xsl:value-of select="$context"/>
    <xsl:if test="$context!='this'">
      <xsl:text>)</xsl:text>
    </xsl:if>
    <xsl:text>.add("</xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text>", </xsl:text>
    <xsl:apply-templates select="void|bound|atom|abstract">
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="parent" select="$parent"/>
      <xsl:with-param name="context" select="$context"/>
    </xsl:apply-templates>
    <xsl:text>);</xsl:text>
  </xsl:template>
  <!-- Void attribute -->
  <xsl:template match="void">
    <xsl:param name="name"/>
    <xsl:text>new AtVoid("</xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text>")</xsl:text>
  </xsl:template>
  <!--
    Atom as attribute.
    We use new Function<>() {...} syntax instead of lambdas from Java 8 because
    1. java does not manage to compile the code with 17+ nested lambdas.
       It just freezes and fails in 5-10 min with heap overflow error.
       So we can't compile nested-blah-test from runtime-tests.eo.
       We haven't reported the bug to openjdk yet, but we will
    2. it just works faster because dynamic dispatch is not happened
  -->
  <xsl:template match="atom">
    <xsl:param name="parent"/>
    <xsl:param name="name"/>
    <xsl:param name="context"/>
    <xsl:param name="indent"/>
    <xsl:variable name="argument" select="o[1]"/>
    <xsl:variable name="class">
      <xsl:value-of select="$parent"/>
      <xsl:value-of select="'$'"/>
      <xsl:value-of select="eo:class-name($name, eo:suffix($argument/@line, $argument/@pos))"/>
    </xsl:variable>
    <xsl:variable name="variable">
      <xsl:if test="$context!='this'">
        <xsl:value-of select="$context"/>
      </xsl:if>
      <xsl:text>atom</xsl:text>
    </xsl:variable>
    <xsl:text>new AtOnce(new AtComposite(</xsl:text>
    <xsl:value-of select="$context"/>
    <xsl:text>, new Function&lt;&gt;() {</xsl:text>
    <xsl:value-of select="eo:eol($indent + 1)"/>
    <xsl:text>@Override</xsl:text>
    <xsl:value-of select="eo:eol($indent + 1)"/>
    <xsl:text>public Phi apply(final Phi </xsl:text>
    <xsl:value-of select="eo:rho($context)"/>
    <xsl:text>) {</xsl:text>
    <xsl:value-of select="eo:eol($indent + 2)"/>
    <xsl:text>Phi </xsl:text>
    <xsl:value-of select="$variable"/>
    <xsl:text> = new </xsl:text>
    <xsl:value-of select="$class"/>
    <xsl:text>();</xsl:text>
    <xsl:apply-templates select="$argument" mode="located">
      <xsl:with-param name="indent" select="$indent + 2"/>
      <xsl:with-param name="name" select="$variable"/>
    </xsl:apply-templates>
    <xsl:value-of select="eo:eol($indent + 2)"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$variable"/>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="eo:eol($indent + 1)"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol($indent)"/>
    <xsl:text>}))</xsl:text>
  </xsl:template>
  <!-- Abstract object as attribute -->
  <xsl:template match="abstract">
    <xsl:param name="parent"/>
    <xsl:param name="name"/>
    <xsl:param name="context"/>
    <xsl:param name="indent"/>
    <xsl:variable name="rho" select="eo:rho($context)"/>
    <xsl:variable name="ctx" select="eo:context($context)"/>
    <xsl:text>new AtOnce(new AtComposite(</xsl:text>
    <xsl:value-of select="$context"/>
    <xsl:text>, new Function&lt;&gt;() {</xsl:text>
    <xsl:value-of select="eo:eol($indent + 1)"/>
    <xsl:text>@Override</xsl:text>
    <xsl:value-of select="eo:eol($indent + 1)"/>
    <xsl:text>public Phi apply(final Phi </xsl:text>
    <xsl:value-of select="$rho"/>
    <xsl:text>) {</xsl:text>
    <xsl:value-of select="eo:eol($indent + 2)"/>
    <xsl:text>Phi </xsl:text>
    <xsl:value-of select="$ctx"/>
    <xsl:text> = new PhDefault();</xsl:text>
    <xsl:apply-templates select="attr">
      <xsl:with-param name="indent" select="$indent + 2"/>
      <xsl:with-param name="parent">
        <xsl:value-of select="$parent"/>
        <xsl:text>$</xsl:text>
        <xsl:value-of select="eo:class-name($name, eo:suffix(@line, @pos))"/>
      </xsl:with-param>
      <xsl:with-param name="context" select="$ctx"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="located">
      <xsl:with-param name="name" select="$ctx"/>
      <xsl:with-param name="indent" select="$indent + 2"/>
    </xsl:apply-templates>
    <xsl:value-of select="eo:eol($indent + 2)"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$ctx"/>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="eo:eol($indent + 1)"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol($indent)"/>
    <xsl:text>}))</xsl:text>
  </xsl:template>
  <!-- Bound attribute -->
  <xsl:template match="bound">
    <xsl:param name="indent"/>
    <xsl:param name="context"/>
    <xsl:variable name="rho" select="eo:rho($context)"/>
    <xsl:variable name="ctx" select="eo:context($context)"/>
    <xsl:text>new AtOnce(new AtComposite(</xsl:text>
    <xsl:value-of select="$context"/>
    <xsl:text>, new Function&lt;&gt;() {</xsl:text>
    <xsl:value-of select="eo:eol($indent + 1)"/>
    <xsl:text>@Override</xsl:text>
    <xsl:value-of select="eo:eol($indent + 1)"/>
    <xsl:text>public Phi apply(final Phi </xsl:text>
    <xsl:value-of select="$rho"/>
    <xsl:text>) {</xsl:text>
    <xsl:apply-templates select="o" mode="object">
      <xsl:with-param name="name" select="$ctx"/>
      <xsl:with-param name="indent" select="$indent + 2"/>
      <xsl:with-param name="rho" select="$rho"/>
    </xsl:apply-templates>
    <xsl:value-of select="eo:eol($indent + 2)"/>
    <xsl:text>return </xsl:text>
    <xsl:value-of select="$ctx"/>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="eo:eol($indent + 1)"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol($indent)"/>
    <xsl:text>}))</xsl:text>
  </xsl:template>
  <!-- Anonymous abstract object -->
  <xsl:template match="o[not(@base) and not(@name)]" mode="object">
    <xsl:param name="indent"/>
    <xsl:param name="name"/>
    <xsl:value-of select="eo:eol($indent)"/>
    <xsl:text>PhDefault </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> = </xsl:text>
    <xsl:choose>
      <xsl:when test="o">
        <xsl:text>new </xsl:text>
        <xsl:value-of select="eo:loc-to-class(@loc)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>new PhDefault</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>();</xsl:text>
  </xsl:template>
  <!-- Attribute body: regular object, not method -->
  <xsl:template match="o[@base and @base!='' and not(starts-with(@base, '.'))]" mode="object">
    <xsl:param name="indent"/>
    <xsl:param name="name"/>
    <xsl:param name="rho"/>
    <xsl:value-of select="eo:eol($indent)"/>
    <xsl:text>Phi </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> = </xsl:text>
    <xsl:choose>
      <xsl:when test="@base='$' or @base='Q'">
        <xsl:value-of select="eo:fqn-start(@base, $rho)"/>
        <xsl:text>;</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="parts" select="tokenize(@base, '\.')"/>
        <xsl:choose>
          <!-- Little optimization -->
          <xsl:when test="starts-with(@base, 'Q.org.eolang')">
            <xsl:value-of select="eo:fqn-start($parts[1], $rho)"/>
            <xsl:for-each select="$parts[position()&gt;1]">
              <xsl:text>.take(</xsl:text>
              <xsl:value-of select="eo:attr-name(., true())"/>
              <xsl:text>)</xsl:text>
            </xsl:for-each>
            <xsl:if test="./value">
              <xsl:text>.copy()</xsl:text>
            </xsl:if>
            <xsl:text>;</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="eo:method(eo:fqn-start($parts[1], $rho), $parts[2])"/>
            <xsl:for-each select="$parts[position()&gt;2]">
              <xsl:value-of select="eo:eol($indent)"/>
              <xsl:value-of select="$name"/>
              <xsl:text> = </xsl:text>
              <xsl:value-of select="eo:method($name, .)"/>
            </xsl:for-each>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="." mode="application">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="rho" select="$rho"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="located">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
  </xsl:template>
  <!-- Attribute body: method object (starts with .) -->
  <xsl:template match="o[starts-with(@base, '.') and *]" mode="object">
    <xsl:param name="indent"/>
    <xsl:param name="name"/>
    <xsl:param name="rho"/>
    <xsl:variable name="method" select="substring-after(@base, '.')"/>
    <xsl:if test="starts-with(@base, concat('.', $eo:alpha))">
      <xsl:message terminate="yes">
        <xsl:text>Dispatching alpha attributes is not supported in EO yet, found: </xsl:text>
        <xsl:value-of select="@base"/>
      </xsl:message>
    </xsl:if>
    <xsl:apply-templates select="o[1]" mode="object">
      <xsl:with-param name="name">
        <xsl:value-of select="$name"/>
        <xsl:text>b</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="rho" select="$rho"/>
    </xsl:apply-templates>
    <xsl:value-of select="eo:eol($indent)"/>
    <xsl:text>Phi </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> = </xsl:text>
    <xsl:value-of select="eo:method(string-join(($name, 'b'), ''), $method)"/>
    <xsl:apply-templates select="." mode="application">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="skip" select="1"/>
      <xsl:with-param name="rho" select="$rho"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="located">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
  </xsl:template>
  <!-- Location of object -->
  <xsl:template match="*" mode="located">
    <xsl:param name="indent"/>
    <xsl:param name="name"/>
    <xsl:if test="@line and @pos">
      <xsl:value-of select="eo:eol($indent)"/>
      <xsl:value-of select="$name"/>
      <xsl:text> = new PhSafe(</xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text>, "</xsl:text>
      <xsl:value-of select="$object-name"/>
      <xsl:text>", </xsl:text>
      <xsl:value-of select="@line"/>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="@pos"/>
      <xsl:text>, </xsl:text>
      <xsl:text>"</xsl:text>
      <xsl:value-of select="@loc"/>
      <xsl:text>"</xsl:text>
      <xsl:text>, "</xsl:text>
      <xsl:value-of select="@original-name"/>
      <xsl:text>");</xsl:text>
    </xsl:if>
  </xsl:template>
  <!-- Application  -->
  <xsl:template match="*" mode="application">
    <xsl:param name="indent"/>
    <xsl:param name="name"/>
    <xsl:param name="rho"/>
    <xsl:param name="skip" select="0"/>
    <xsl:variable name="inners" select="o[position() &gt; $skip and not(@level)]"/>
    <xsl:for-each select="$inners">
      <xsl:if test="position() = 1">
        <xsl:value-of select="eo:eol($indent)"/>
        <xsl:value-of select="$name"/>
        <xsl:text> = </xsl:text>
        <xsl:text>new PhCopy(</xsl:text>
        <xsl:value-of select="$name"/>
        <xsl:text>);</xsl:text>
      </xsl:if>
      <xsl:variable name="next">
        <xsl:value-of select="$name"/>
        <xsl:value-of select="position()"/>
      </xsl:variable>
      <xsl:apply-templates select="." mode="object">
        <xsl:with-param name="name" select="$next"/>
        <xsl:with-param name="indent" select="$indent + 1"/>
        <xsl:with-param name="rho" select="$rho"/>
      </xsl:apply-templates>
    </xsl:for-each>
    <xsl:for-each select="$inners">
      <xsl:value-of select="eo:eol($indent)"/>
      <xsl:value-of select="$name"/>
      <xsl:text> = new PhWith(</xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text>, </xsl:text>
      <xsl:choose>
        <xsl:when test="@as">
          <xsl:value-of select="eo:attr-name(@as, true())"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="position() - 1"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:value-of select="position()"/>
      <xsl:text>);</xsl:text>
    </xsl:for-each>
    <xsl:apply-templates select="value">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent">
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:template>
  <!-- Data -->
  <xsl:template match="value">
    <xsl:param name="indent"/>
    <xsl:param name="name"/>
    <xsl:value-of select="eo:eol($indent)"/>
    <xsl:value-of select="$name"/>
    <xsl:text> = new PhWith(</xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text>, 0, new PhDefault(</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>));</xsl:text>
  </xsl:template>
  <!-- Class for tests -->
  <xsl:template match="class" mode="tests">
    <xsl:value-of select="eo:eol(1)"/>
    <xsl:for-each select="attr">
      <xsl:if test="position()&gt;1">
        <xsl:value-of select="eo:eol(1)"/>
      </xsl:if>
      <xsl:text>@Test</xsl:text>
      <xsl:value-of select="eo:eol(1)"/>
      <xsl:text>void </xsl:text>
      <xsl:value-of select="replace(@name, '-', '_')"/>
      <xsl:text>() throws java.lang.Exception {</xsl:text>
      <xsl:value-of select="eo:eol(2)"/>
      <xsl:choose>
        <xsl:when test="starts-with(@name, 'throws')">
          <xsl:text>Assertions.assertThrows(Exception.class, () -&gt; {</xsl:text>
          <xsl:apply-templates select="." mode="dataized">
            <xsl:with-param name="indent" select="3"/>
          </xsl:apply-templates>
          <xsl:text>;</xsl:text>
          <xsl:value-of select="eo:eol(2)"/>
          <xsl:text>});</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>Assertions.assertTrue(</xsl:text>
          <xsl:apply-templates select="." mode="dataized">
            <xsl:with-param name="indent" select="3"/>
          </xsl:apply-templates>
          <xsl:value-of select="eo:eol(2)"/>
          <xsl:text>);</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:value-of select="eo:eol(1)"/>
      <xsl:text>}</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    </xsl:for-each>
  </xsl:template>
  <!-- Dataize test -->
  <xsl:template match="attr" mode="dataized">
    <xsl:param name="indent"/>
    <xsl:value-of select="eo:eol($indent)"/>
    <xsl:text>new Dataized(this.take(</xsl:text>
    <xsl:value-of select="eo:attr-name(@name, true())"/>
    <xsl:text>)).asBool()</xsl:text>
  </xsl:template>
  <!-- Package -->
  <xsl:template match="meta[head='package']" mode="head">
    <xsl:text>package </xsl:text>
    <xsl:value-of select="eo:class-name(tail, '')"/>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <!-- Imports for tests  -->
  <xsl:template match="meta[head='tests']" mode="head">
    <xsl:text>import org.junit.jupiter.api.Assertions;</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text>import org.junit.jupiter.api.Test;</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <!-- License with disclaimer  -->
  <xsl:template match="/object" mode="license">
    <xsl:text>/* </xsl:text>
    <xsl:value-of select="$disclaimer"/>
    <xsl:text> */</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <!-- Other -->
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
