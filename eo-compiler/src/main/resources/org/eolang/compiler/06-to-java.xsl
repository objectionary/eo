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
  <xsl:variable name="EOL">
    <xsl:text>
</xsl:text>
  </xsl:variable>
  <xsl:variable name="TAB">
    <xsl:text>  </xsl:text>
  </xsl:variable>
  <xsl:function name="eo:name" as="xs:string">
    <xsl:param name="n" as="xs:string"/>
    <xsl:sequence select="replace($n, '\+', '_')"/>
  </xsl:function>
  <xsl:template match="/program/objects/o[@name and not(@base)]">
    <xsl:variable name="methods" select="./o[not(@name and not(@base) and not(./o))]"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="java">
        <xsl:value-of select="$EOL"/>
        <xsl:text>import org.eolang.*;</xsl:text>
        <xsl:value-of select="$EOL"/>
        <xsl:text>public final class </xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:if test="o[not(@name)]">
          <xsl:text> implements java.util.concurrent.Callable&lt;Object&gt;</xsl:text>
        </xsl:if>
        <xsl:text> {</xsl:text>
        <xsl:value-of select="$EOL"/>
        <xsl:value-of select="$TAB"/>
        <xsl:text>private final Args args;</xsl:text>
        <xsl:value-of select="$EOL"/>
        <xsl:value-of select="$TAB"/>
        <xsl:text>public </xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>(final Args a) {</xsl:text>
        <xsl:value-of select="$EOL"/>
        <xsl:for-each select="./o[@name and not(@base)]">
          <xsl:value-of select="$TAB"/>
          <xsl:value-of select="$TAB"/>
          <xsl:text>assert a.has("</xsl:text>
          <xsl:value-of select="@name"/>
          <xsl:text>");</xsl:text>
          <xsl:value-of select="$EOL"/>
        </xsl:for-each>
        <xsl:value-of select="$TAB"/>
        <xsl:value-of select="$TAB"/>
        <xsl:text>this.args = a;</xsl:text>
        <xsl:value-of select="$EOL"/>
        <xsl:value-of select="$TAB"/>
        <xsl:text>}</xsl:text>
        <xsl:value-of select="$EOL"/>
        <xsl:for-each select="$methods">
          <xsl:apply-templates select="." mode="method"/>
        </xsl:for-each>
        <xsl:text>}</xsl:text>
        <xsl:value-of select="$EOL"/>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
  <xsl:template match="o" mode="method">
    <xsl:value-of select="$TAB"/>
    <xsl:if test="not(@name)">
      <xsl:text>@Override</xsl:text>
      <xsl:value-of select="$EOL"/>
      <xsl:value-of select="$TAB"/>
      <xsl:text>public Object call() throws Exception</xsl:text>
      <xsl:value-of select="@name"/>
    </xsl:if>
    <xsl:if test="@name">
      <xsl:text>public Object </xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>(final Args a)</xsl:text>
    </xsl:if>
    <xsl:text> {</xsl:text>
    <xsl:value-of select="$EOL"/>
    <xsl:for-each select="descendant-or-self::o[@name]">
      <xsl:value-of select="$TAB"/>
      <xsl:value-of select="$TAB"/>
      <xsl:text>Object </xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>;</xsl:text>
      <xsl:value-of select="$EOL"/>
    </xsl:for-each>
    <xsl:value-of select="$TAB"/>
    <xsl:value-of select="$TAB"/>
    <xsl:text>return </xsl:text>
    <xsl:apply-templates select=".">
      <xsl:with-param name="indent">
        <xsl:value-of select="$TAB"/>
        <xsl:value-of select="$TAB"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:text>;</xsl:text>
    <xsl:value-of select="$EOL"/>
    <xsl:value-of select="$TAB"/>
    <xsl:text>}</xsl:text>
    <xsl:value-of select="$EOL"/>
  </xsl:template>
  <xsl:template match="o[@base and @ref]">
    <xsl:text>this.args.get("</xsl:text>
    <xsl:value-of select="@base"/>
    <xsl:text>")</xsl:text>
  </xsl:template>
  <xsl:template match="o[starts-with(@base, '.') and ./o]">
    <xsl:param name="indent"/>
    <xsl:if test="@name">
      <xsl:value-of select="@name"/>
      <xsl:text> = </xsl:text>
    </xsl:if>
    <xsl:apply-templates select="./o[1]">
      <xsl:with-param name="indent">
        <xsl:value-of select="$indent"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:value-of select="@base"/>
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="./o[position() &gt; 1]">
      <xsl:with-param name="indent">
        <xsl:value-of select="$indent"/>
      </xsl:with-param>
    </xsl:apply-templates>
    <xsl:text>)</xsl:text>
  </xsl:template>
  <xsl:template match="o[@base and not(starts-with(@base, '.')) and not(@ref) and not(text())]">
    <xsl:param name="indent"/>
    <xsl:variable name="newindent">
      <xsl:value-of select="$indent"/>
      <xsl:value-of select="$TAB"/>
    </xsl:variable>
    <xsl:if test="@name">
      <xsl:value-of select="@name"/>
      <xsl:text> = </xsl:text>
    </xsl:if>
    <xsl:text>new </xsl:text>
    <xsl:value-of select="replace(@base, '^org\.eolang\.([A-Za-z]+)$', '$1')"/>
    <xsl:text>(new ArgsOf(</xsl:text>
    <xsl:if test="ancestor-or-self::o[parent::o/parent::objects and @name]">
      <xsl:text>a</xsl:text>
    </xsl:if>
    <xsl:if test="./o">
      <xsl:text>,</xsl:text>
      <xsl:value-of select="$EOL"/>
      <xsl:value-of select="$newindent"/>
    </xsl:if>
    <xsl:for-each select="./o">
      <xsl:text>new Entry("</xsl:text>
      <xsl:if test="@as">
        <xsl:value-of select="@as"/>
      </xsl:if>
      <xsl:if test="not(@as)">
        <xsl:value-of select="format-number(position(), '00')"/>
      </xsl:if>
      <xsl:text>", </xsl:text>
      <xsl:apply-templates select=".">
        <xsl:with-param name="indent">
          <xsl:value-of select="$newindent"/>
        </xsl:with-param>
      </xsl:apply-templates>
      <xsl:text>)</xsl:text>
      <xsl:if test="position() != last()">
        <xsl:text>,</xsl:text>
        <xsl:value-of select="$EOL"/>
        <xsl:value-of select="$newindent"/>
      </xsl:if>
    </xsl:for-each>
    <xsl:if test="./o">
      <xsl:value-of select="$EOL"/>
      <xsl:value-of select="$indent"/>
    </xsl:if>
    <xsl:text>))</xsl:text>
  </xsl:template>
  <xsl:template match="o[text() and @base='org.eolang.float' or @base='org.eolang.integer' or @base='org.eolang.hex']">
    <xsl:value-of select="text()"/>
  </xsl:template>
  <xsl:template match="o[text() and @base='org.eolang.string']">
    <xsl:text>"</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>"</xsl:text>
  </xsl:template>
  <xsl:template match="o[text() and @base='org.eolang.char']">
    <xsl:text>'</xsl:text>
    <xsl:value-of select="text()"/>
    <xsl:text>'</xsl:text>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
