<?xml version="1.0"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2020 Yegor Bugayenko

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:eo="https://www.eolang.org" xmlns:xs="http://www.w3.org/2001/XMLSchema" version="2.0">
  <xsl:strip-space elements="*"/>
  <xsl:import href="_funcs.xsl"/>
  <xsl:variable name="TAB">
    <xsl:text>  </xsl:text>
  </xsl:variable>
  <xsl:function name="eo:eol">
    <xsl:param name="tabs"/>
    <xsl:value-of select="'&#10;'"/>
    <xsl:value-of select="eo:tabs($tabs)"/>
  </xsl:function>
  <xsl:function name="eo:tabs">
    <xsl:param name="n"/>
    <xsl:for-each select="1 to $n">
      <xsl:text>  </xsl:text>
    </xsl:for-each>
  </xsl:function>
  <xsl:function name="eo:type-of">
    <xsl:param name="root"/>
    <xsl:param name="o"/>
    <xsl:choose>
      <xsl:when test="$o/@base and $o/@ref">
        <xsl:copy-of select="eo:type-of($root, $root//o[@name=$o/@base and @line=$o/@ref])"/>
      </xsl:when>
      <xsl:when test="not($o/@base) and not($o/@ref) and contains($o/@line, '.')">
        <xsl:copy-of select="eo:type-of($root, $root//o[@line=substring-after($o/@line, '.') and @name=$o/@name])"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$o"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:function name="eo:class-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:variable name="parts" select="tokenize($n, '\.')"/>
    <xsl:variable name="p">
      <xsl:for-each select="$parts">
        <xsl:if test="position()!=last()">
          <xsl:value-of select="."/>
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
    <xsl:value-of select="concat($p, 'EO', replace(replace($c, '@', '_origin'), '\$', '\$EO'))"/>
  </xsl:function>
  <xsl:function name="eo:attr-name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:choose>
      <xsl:when test="$n='@'">
        <xsl:text>_origin</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat('', $n)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>
  <xsl:template match="@name">
    <xsl:attribute name="name">
      <xsl:value-of select="."/>
    </xsl:attribute>
    <xsl:attribute name="java-name">
      <xsl:value-of select="eo:class-name(.)"/>
    </xsl:attribute>
  </xsl:template>
  <xsl:template match="o[eo:abstract(.) and not(@atom)]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="java">
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:apply-templates select="/program" mode="license"/>
        <xsl:apply-templates select="/program/metas/meta[head='package']"/>
        <xsl:text>import org.eolang.*;</xsl:text>
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:text>public final class </xsl:text>
        <xsl:value-of select="eo:class-name(@name)"/>
        <xsl:text> extends Phi {</xsl:text>
        <xsl:value-of select="eo:eol(0)"/>
        <xsl:apply-templates select="." mode="ctor"/>
        <xsl:text>}</xsl:text>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o" mode="ctor">
    <xsl:value-of select="eo:tabs(1)"/>
    <xsl:text>public </xsl:text>
    <xsl:value-of select="eo:class-name(@name)"/>
    <xsl:text>() {</xsl:text>
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:text>super(</xsl:text>
    <xsl:for-each select="o[@name and (not(@base) or @rw) and not(@level)]">
      <xsl:if test="position() &gt; 1">
        <xsl:text>, </xsl:text>
      </xsl:if>
      <xsl:text>"</xsl:text>
      <xsl:value-of select="eo:attr-name(@name)"/>
      <xsl:text>"</xsl:text>
    </xsl:for-each>
    <xsl:text>);</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:apply-templates select="o[@name and @base]" mode="attr">
      <xsl:with-param name="indent">
        <xsl:value-of select="eo:tabs(2)"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:value-of select="eo:tabs(1)"/>
    <xsl:text>};</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="o" mode="attr">
    <xsl:value-of select="eo:tabs(2)"/>
    <xsl:text>this.put("</xsl:text>
    <xsl:value-of select="eo:attr-name(@name)"/>
    <xsl:text>", () -&gt; {</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:apply-templates select=".">
      <xsl:with-param name="name" select="'ret'"/>
      <xsl:with-param name="indent">
        <xsl:value-of select="eo:tabs(3)"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:value-of select="eo:tabs(3)"/>
    <xsl:text>return ret;</xsl:text>
    <xsl:value-of select="eo:eol(2)"/>
    <xsl:text>});</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="o[@base and not(starts-with(@base, '.'))]">
    <xsl:param name="indent"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:variable name="o" select="."/>
    <xsl:variable name="b" select="//o[@name=$o/@base and @line=$o/@ref]"/>
    <xsl:value-of select="$indent"/>
    <xsl:text>final Phi </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> = </xsl:text>
    <xsl:choose>
      <xsl:when test="@base='$'">
        <xsl:text>this</xsl:text>
      </xsl:when>
      <xsl:when test="$b and eo:abstract($b)">
        <xsl:text>new </xsl:text>
        <xsl:value-of select="eo:class-name($b/@name)"/>
        <xsl:text>().inherit(this)</xsl:text>
      </xsl:when>
      <xsl:when test="@ref">
        <xsl:text>this.get("</xsl:text>
        <xsl:value-of select="eo:attr-name(@base)"/>
        <xsl:text>").copy()</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:text>new </xsl:text>
        <xsl:value-of select="eo:class-name(@base)"/>
        <xsl:text>().inherit(this)</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:apply-templates select=".[@data]" mode="data">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="." mode="application">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="o[starts-with(@base, '.') and o]">
    <xsl:param name="indent"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:apply-templates select="o[1]">
      <xsl:with-param name="name">
        <xsl:value-of select="$name"/>
        <xsl:text>_base</xsl:text>
      </xsl:with-param>
      <xsl:with-param name="indent">
        <xsl:value-of select="$indent"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:value-of select="$indent"/>
    <xsl:text>final Phi </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text> = </xsl:text>
    <xsl:value-of select="$name"/>
    <xsl:text>_base.get("</xsl:text>
    <xsl:value-of select="eo:attr-name(substring-after(@base, '.'))"/>
    <xsl:text>").copy();</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:apply-templates select="." mode="application">
      <xsl:with-param name="name" select="$name"/>
      <xsl:with-param name="indent" select="$indent"/>
      <xsl:with-param name="skip" select="1"/>
    </xsl:apply-templates>
  </xsl:template>
  <xsl:template match="o" mode="application">
    <xsl:param name="indent"/>
    <xsl:param name="skip" select="0"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:for-each select="o[position() &gt; $skip][not(@level)]">
      <xsl:variable name="n">
        <xsl:value-of select="$name"/>
        <xsl:text>_</xsl:text>
        <xsl:value-of select="position()"/>
      </xsl:variable>
      <xsl:apply-templates select=".">
        <xsl:with-param name="name" select="$n"/>
        <xsl:with-param name="indent">
          <xsl:value-of select="$indent"/>
          <xsl:value-of select="eo:tabs(1)"/>
        </xsl:with-param>
      </xsl:apply-templates>
    </xsl:for-each>
    <xsl:for-each select="o[position() &gt; $skip][not(@level)]">
      <xsl:value-of select="$indent"/>
      <xsl:value-of select="eo:tabs(1)"/>
      <xsl:value-of select="$name"/>
      <xsl:text>.put(</xsl:text>
      <xsl:choose>
        <xsl:when test="@as">
          <xsl:text>"</xsl:text>
          <xsl:value-of select="@as"/>
          <xsl:text>"</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="position() - 1"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>, </xsl:text>
      <xsl:value-of select="$name"/>
      <xsl:text>_</xsl:text>
      <xsl:value-of select="position()"/>
      <xsl:text>);</xsl:text>
      <xsl:value-of select="eo:eol(0)"/>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="o[@data]" mode="data">
    <xsl:param name="indent"/>
    <xsl:param name="name" select="'o'"/>
    <xsl:value-of select="$indent"/>
    <xsl:value-of select="$name"/>
    <xsl:text>.put("_data", new Data.Value&lt;</xsl:text>
    <xsl:apply-templates select="." mode="value"/>
    <xsl:text>);</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="o[@data='string']" mode="value">
    <xsl:text>String&gt;("</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>")</xsl:text>
  </xsl:template>
  <xsl:template match="o[@data='int']" mode="value">
    <xsl:text>Integer&gt;(</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="o[@data='bool']" mode="value">
    <xsl:text>Boolean&gt;(</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="o[@data='float']" mode="value">
    <xsl:text>Double&gt;(</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="/program/metas/meta[head='package']" mode="#all">
    <xsl:text>package </xsl:text>
    <xsl:value-of select="tail"/>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="/program" mode="license">
    <xsl:text>/*</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text> * This file was auto-generated by eolang-maven-plugin</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text> * on </xsl:text>
    <xsl:value-of select="current-dateTime()"/>
    <xsl:text>. Don't edit it,</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text> * your changes will be discarded on the next build.</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text> * The sources were compiled to XML on</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text> * </xsl:text>
    <xsl:value-of select="@time"/>
    <xsl:text> by the EO compiler v.</xsl:text>
    <xsl:value-of select="@version"/>
    <xsl:text>.</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:text> */</xsl:text>
    <xsl:value-of select="eo:eol(0)"/>
    <xsl:value-of select="eo:eol(0)"/>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
