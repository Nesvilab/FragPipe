/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.utils.swing;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import net.miginfocom.layout.AnimSpec;
import net.miginfocom.layout.BoundSize;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.DimConstraint;
import net.miginfocom.layout.LC;
import net.miginfocom.layout.UnitValue;

/**
 * Extension methods to CC class from MigLayout. Most methods are just delegating to the original
 * class as {@link CC} is marked final, so we can't extend it.
 */
public class CC2 {

  private CC cc;

  public CC2(CC cc) {
    this.cc = cc;
  }

  public CC2() {
    cc = new CC();
  }

  public CC get() {
    return cc;
  }

  /* Shortcut for 'new'. */
  public static CC2 n() {
    return new CC2();
  }

  /* Shortcut for 'alignX(left)'. */
  public CC2 xL() {
    cc = cc.alignX("left");
    return this;
  }

  /* Shortcut for 'alignX(right)'. */
  public CC2 xR() {
    cc = cc.alignX("right");
    return this;
  }

  /* Shortcut for 'wrap'. */
  public CC2 w() {
    cc = cc.wrap();
    return this;
  }

  /* Shortcut for 'growX'. */
  public CC2 gX() {
    cc = cc.growX();
    return this;
  }


  /**
   * Specifies that the component should be put in the end group <code>s</code> and will thus share
   * the same ending coordinate as them within the group.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param s A name to associate on the group that should be the same for other rows/columns in the
   *          same group.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 endGroupX(String s) {
    cc = cc.endGroupX(s);

    return this;
  }

  /**
   * Specifies that the component should be put in the size group <code>s</code> and will thus share
   * the same size as them within the group.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param s A name to associate on the group that should be the same for other rows/columns in the
   *          same group.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 sizeGroupX(String s) {
    cc = cc.sizeGroupX(s);

    return this;
  }

  /**
   * The minimum size for the component. The value will override any value that is set on the
   * component itself.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param size The size expressed as a <code>UnitValue</code>. E.g. "100px" or "200mm".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 minWidth(String size) {
    cc = cc.minWidth(size);

    return this;
  }

  /**
   * The size for the component as a min and/or preferred and/or maximum size. The value will
   * override any value that is set on the component itself.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param size The size expressed as a <code>BoundSize</code>. E.g. "50:100px:200mm" or "100px".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 width(String size) {
    cc = cc.width(size);

    return this;
  }

  /**
   * The maximum size for the component. The value will override any value that is set on the
   * component itself.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param size The size expressed as a <code>UnitValue</code>. E.g. "100px" or "200mm".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 maxWidth(String size) {
    cc = cc.maxWidth(size);

    return this;
  }

  /**
   * The horizontal gap before and/or after the component. The gap is towards cell bounds and/or
   * other component bounds.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param before The size of the gap expressed as a <code>BoundSize</code>. E.g. "50:100px:200mm"
   *               or "100px!".
   * @param after  The size of the gap expressed as a <code>BoundSize</code>. E.g. "50:100px:200mm"
   *               or "100px!".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 gapX(String before, String after) {
    cc = cc.gapX(before, after);

    return this;
  }

  /**
   * Same functionality as <code>getHorizontal().setAlign(ConstraintParser.parseUnitValue(unitValue,
   * true))</code> only this method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param align The align keyword or for instance "100px". E.g "left", "right", "leading" or
   *              "trailing".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 alignX(String align) {
    cc = cc.alignX(align);

    return this;
  }

  /**
   * The grow priority compared to other components in the same cell.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param p The grow priority.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 growPrioX(int p) {
    cc = cc.growPrioX(p);

    return this;
  }

  /**
   * Grow priority for the component horizontally and optionally vertically.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param widthHeight The new shrink weight and height. 1-2 arguments, never null.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 growPrio(int... widthHeight) {
    cc = cc.growPrio(widthHeight);

    return this;
  }

  /**
   * Grow weight for the component horizontally. It default to weight <code>100</code>.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #growX(float)
   */
  public CC2 growX() {
    cc = cc.growX();

    return this;
  }

  /**
   * Grow weight for the component horizontally.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param w The new grow weight.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 growX(float w) {
    cc = cc.growX(w);

    return this;
  }

  /**
   * grow weight for the component horizontally and optionally vertically.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param widthHeight The new shrink weight and height. 1-2 arguments, never null.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 grow(float... widthHeight) {
    cc = cc.grow(widthHeight);

    return this;
  }

  /**
   * The shrink priority compared to other components in the same cell.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param p The shrink priority.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 shrinkPrioX(int p) {
    cc = cc.shrinkPrioX(p);

    return this;
  }

  /**
   * Shrink priority for the component horizontally and optionally vertically.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param widthHeight The new shrink weight and height. 1-2 arguments, never null.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 shrinkPrio(int... widthHeight) {
    cc = cc.shrinkPrio(widthHeight);

    return this;
  }

  /**
   * Shrink weight for the component horizontally.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param w The new shrink weight.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 shrinkX(float w) {
    cc = cc.shrinkX(w);

    return this;
  }

  /**
   * Shrink weight for the component horizontally and optionally vertically.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param widthHeight The new shrink weight and height. 1-2 arguments, never null.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 shrink(float... widthHeight) {
    cc = cc.shrink(widthHeight);

    return this;
  }

  /**
   * The end group that this component should be placed in.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param s The name of the group. If <code>null</code> that means no group (default)
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 endGroupY(String s) {
    cc = cc.endGroupY(s);

    return this;
  }

  /**
   * The end group(s) that this component should be placed in.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param xy The end group for x and y respectively. 1-2 arguments, not null.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 endGroup(String... xy) {
    cc = cc.endGroup(xy);

    return this;
  }

  /**
   * The size group that this component should be placed in.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param s The name of the group. If <code>null</code> that means no group (default)
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 sizeGroupY(String s) {
    cc = cc.sizeGroupY(s);

    return this;
  }

  /**
   * The size group(s) that this component should be placed in.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param xy The size group for x and y respectively. 1-2 arguments, not null.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 sizeGroup(String... xy) {
    cc = cc.sizeGroup(xy);

    return this;
  }

  /**
   * The minimum size for the component. The value will override any value that is set on the
   * component itself.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param size The size expressed as a <code>UnitValue</code>. E.g. "100px" or "200mm".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 minHeight(String size) {
    cc = cc.minHeight(size);

    return this;
  }

  /**
   * The size for the component as a min and/or preferred and/or maximum size. The value will
   * override any value that is set on the component itself.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param size The size expressed as a <code>BoundSize</code>. E.g. "50:100px:200mm" or "100px".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 height(String size) {
    cc = cc.height(size);

    return this;
  }

  /**
   * The maximum size for the component. The value will override any value that is set on the
   * component itself.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param size The size expressed as a <code>UnitValue</code>. E.g. "100px" or "200mm".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 maxHeight(String size) {
    cc = cc.maxHeight(size);

    return this;
  }

  /**
   * The vertical gap before (normally above) and/or after (normally below) the component. The gap
   * is towards cell bounds and/or other component bounds.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param before The size of the gap expressed as a <code>BoundSize</code>. E.g. "50:100px:200mm"
   *               or "100px!".
   * @param after  The size of the gap expressed as a <code>BoundSize</code>. E.g. "50:100px:200mm"
   *               or "100px!".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 gapY(String before, String after) {
    cc = cc.gapY(before, after);

    return this;
  }

  /**
   * Same functionality as <code>getVertical().setAlign(ConstraintParser.parseUnitValue(unitValue,
   * true))</code> only this method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param align The align keyword or for instance "100px". E.g "top" or "bottom".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 alignY(String align) {
    cc = cc.alignY(align);

    return this;
  }

  /**
   * The grow priority compared to other components in the same cell.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param p The grow priority.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 growPrioY(int p) {
    cc = cc.growPrioY(p);

    return this;
  }

  /**
   * Grow weight for the component vertically. Defaults to <code>100</code>.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #growY(Float)
   */
  public CC2 growY() {
    cc = cc.growY();

    return this;
  }

  /**
   * Grow weight for the component vertically.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param w The new grow weight.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 growY(float w) {
    cc = cc.growY(w);

    return this;
  }

  /**
   * Grow weight for the component vertically.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param w The new grow weight.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  @Deprecated
  public CC2 growY(Float w) {
    cc = cc.growY(w);

    return this;
  }

  /**
   * The shrink priority compared to other components in the same cell.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param p The shrink priority.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 shrinkPrioY(int p) {
    cc = cc.shrinkPrioY(p);

    return this;
  }

  /**
   * Shrink weight for the component horizontally.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param w The new shrink weight.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 shrinkY(float w) {
    cc = cc.shrinkY(w);

    return this;
  }

  /**
   * How this component, if hidden (not visible), should be treated.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param mode The mode. Default to the mode in the {@link LC}. 0 == Normal. Bounds will be
   *             calculated as if the component was visible.<br> 1 == If hidden the size will be 0,
   *             0 but the gaps remain.<br> 2 == If hidden the size will be 0, 0 and gaps set to
   *             zero.<br> 3 == If hidden the component will be disregarded completely and not take
   *             up a cell in the grid..
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 hideMode(int mode) {
    cc = cc.hideMode(mode);

    return this;
  }

  /**
   * The id used to reference this component in some constraints.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param s The id or <code>null</code>. May consist of a groupID and an componentID which are
   *          separated by a dot: ".". E.g. "grp1.id1". The dot should never be first or last if
   *          present.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   */
  public CC2 id(String s) {
    cc = cc.id(s);

    return this;
  }

  /**
   * Same functionality as {@link #setTag(String tag)} only this method returns <code>this</code>
   * for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param tag The new tag. May be <code>null</code>.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setTag(String)
   */
  public CC2 tag(String tag) {
    cc = cc.tag(tag);

    return this;
  }

  /**
   * Set the cell(s) that the component should occupy in the grid. Same functionality as {@link
   * #setCellX(int col)} and {@link #setCellY(int row)} together with {@link #setSpanX(int width)}
   * and {@link #setSpanY(int height)}. This method returns <code>this</code> for chaining multiple
   * calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param colRowWidthHeight cellX, cellY, spanX, spanY respectively. 1-4 arguments, not null.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setCellX(int)
   * @see #setCellY(int)
   * @see #setSpanX(int)
   * @see #setSpanY(int)
   * @since 3.7.2. Replacing cell(int, int) and cell(int, int, int, int)
   */
  public CC2 cell(int... colRowWidthHeight) {
    cc = cc.cell(colRowWidthHeight);

    return this;
  }

  /**
   * Same functionality as <code>spanX(cellsX).spanY(cellsY)</code> which means this cell will span
   * cells in both x and y. This method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com. Since 3.7.2 this takes an array/vararg whereas it previously only
   * took two specific values, xSpan and ySpan.
   *
   * @param cells spanX and spanY, when present, and in that order.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setSpanY(int)
   * @see #setSpanX(int)
   * @see #spanY()
   * @see #spanX()
   * @since 3.7.2 Replaces span(int, int).
   */
  public CC2 span(int... cells) {
    cc = cc.span(cells);

    return this;
  }

  /**
   * Corresponds exactly to the "gap left right top bottom" keyword.
   *
   * @param args Same as for the "gap" keyword. Length 1-4, never null buf elements can be null.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 gap(String... args) {
    cc = cc.gap(args);

    return this;
  }

  /**
   * Sets the horizontal gap before the component.
   * <p>
   * Note! This is currently same as gapLeft(). This might change in 4.x.
   *
   * @param boundsSize The size of the gap expressed as a <code>BoundSize</code>. E.g.
   *                   "50:100px:200mm" or "100px!".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 gapBefore(String boundsSize) {
    cc = cc.gapBefore(boundsSize);

    return this;
  }

  /**
   * Sets the horizontal gap after the component.
   * <p>
   * Note! This is currently same as gapRight(). This might change in 4.x.
   *
   * @param boundsSize The size of the gap expressed as a <code>BoundSize</code>. E.g.
   *                   "50:100px:200mm" or "100px!".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 gapAfter(String boundsSize) {
    cc = cc.gapAfter(boundsSize);

    return this;
  }

  /**
   * Sets the gap above the component.
   *
   * @param boundsSize The size of the gap expressed as a <code>BoundSize</code>. E.g.
   *                   "50:100px:200mm" or "100px!".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 gapTop(String boundsSize) {
    cc = cc.gapTop(boundsSize);

    return this;
  }

  /**
   * Sets the gap to the left the component.
   *
   * @param boundsSize The size of the gap expressed as a <code>BoundSize</code>. E.g.
   *                   "50:100px:200mm" or "100px!".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 gapLeft(String boundsSize) {
    cc = cc.gapLeft(boundsSize);

    return this;
  }

  /**
   * Sets the gap below the component.
   *
   * @param boundsSize The size of the gap expressed as a <code>BoundSize</code>. E.g.
   *                   "50:100px:200mm" or "100px!".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 gapBottom(String boundsSize) {
    cc = cc.gapBottom(boundsSize);

    return this;
  }

  /**
   * Sets the gap to the right of the component.
   *
   * @param boundsSize The size of the gap expressed as a <code>BoundSize</code>. E.g.
   *                   "50:100px:200mm" or "100px!".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @since 3.7.2
   */
  public CC2 gapRight(String boundsSize) {
    cc = cc.gapRight(boundsSize);

    return this;
  }

  /**
   * Same functionality as calling {@link #setSpanY(int)} with <code>LayoutUtil.INF</code> which
   * means this cell will span the rest of the column. This method returns <code>this</code> for
   * chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setSpanY(int)
   * @see #spanY()
   */
  public CC2 spanY() {
    cc = cc.spanY();

    return this;
  }

  /**
   * Same functionality as {@link #setSpanY(int)} only this method returns <code>this</code> for
   * chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param cells The number of cells to span (i.e. merge).
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setSpanY(int)
   */
  public CC2 spanY(int cells) {
    cc = cc.spanY(cells);

    return this;
  }

  /**
   * Same functionality as {@link #setSpanX(int)} which means this cell will span the rest of the
   * row. This method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setSpanX(int)
   * @see #spanX()
   */
  public CC2 spanX() {
    cc = cc.spanX();

    return this;
  }

  /**
   * Same functionality as {@link #setSpanX(int)} only this method returns <code>this</code> for
   * chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param cells The number of cells to span (i.e. merge).
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setSpanY(int)
   */
  public CC2 spanX(int cells) {
    cc = cc.spanX(cells);

    return this;
  }

  /**
   * Same functionality as <code>pushX().pushY()</code> which means this cell will push in both x
   * and y dimensions. This method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setPushX(Float)
   * @see #setPushX(Float)
   * @see #pushY()
   * @see #pushX()
   */
  public CC2 push() {
    cc = cc.push();

    return this;
  }

  /**
   * Same functionality as <code>pushX(weightX).pushY(weightY)</code> which means this cell will
   * push in both x and y dimensions. This method returns <code>this</code> for chaining multiple
   * calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param weightX The weight used in the push.
   * @param weightY The weight used in the push.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setPushY(Float)
   * @see #setPushX(Float)
   * @see #pushY()
   * @see #pushX()
   */
  public CC2 push(Float weightX, Float weightY) {
    cc = cc.push(weightX, weightY);

    return this;
  }

  /**
   * Same functionality as {@link #setPushY(Float)} which means this cell will push the rest of the
   * column. This method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setPushY(Float)
   */
  public CC2 pushY() {
    cc = cc.pushY();

    return this;
  }

  /**
   * Same functionality as {@link #setPushY(Float weight)} only this method returns
   * <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param weight The weight used in the push.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setPushY(Float)
   */
  public CC2 pushY(Float weight) {
    cc = cc.pushY(weight);

    return this;
  }

  /**
   * Same functionality as {@link #setPushX(Float)} which means this cell will push the rest of the
   * row. This method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setPushX(Float)
   */
  public CC2 pushX() {
    cc = cc.pushX();

    return this;
  }

  /**
   * Same functionality as {@link #setPushX(Float weight)} only this method returns
   * <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param weight The weight used in the push.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     LayoutConstraint().noGrid().gap().fill()</code>.
   * @see #setPushY(Float)
   */
  public CC2 pushX(Float weight) {
    cc = cc.pushX(weight);

    return this;
  }

  /**
   * Same functionality as {@link #setSplit(int parts)} only this method returns <code>this</code>
   * for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param parts The number of parts (i.e. component slots) the cell should be divided into.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setSplit(int)
   */
  public CC2 split(int parts) {
    cc = cc.split(parts);

    return this;
  }

  /**
   * Same functionality as split(LayoutUtil.INF), which means split until one of the keywords that
   * breaks the split is found for a component after this one (e.g. wrap, newline and skip).
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setSplit(int)
   * @since 3.7.2
   */
  public CC2 split() {
    cc = cc.split();

    return this;
  }

  /**
   * Same functionality as {@link #setSkip(int)} only this method returns <code>this</code> for
   * chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param cells How many cells in the grid that should be skipped <b>before</b> the component that
   *              this constraint belongs to
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setSkip(int)
   */
  public CC2 skip(int cells) {
    cc = cc.skip(cells);

    return this;
  }

  /**
   * Same functionality as skip(1).
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setSkip(int)
   * @since 3.7.2
   */
  public CC2 skip() {
    cc = cc.skip();

    return this;
  }

  /**
   * Same functionality as calling {@link #setExternal(boolean)} with <code>true</code> only this
   * method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setExternal(boolean)
   */
  public CC2 external() {
    cc = cc.external();

    return this;
  }

  /**
   * Same functionality as calling {@link #setFlowX(Boolean)} with <code>Boolean.TRUE</code> only
   * this method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setFlowX(Boolean)
   */
  public CC2 flowX() {
    cc = cc.flowX();

    return this;
  }

  /**
   * Same functionality as calling {@link #setFlowX(Boolean)} with <code>Boolean.FALSE</code> only
   * this method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setFlowX(Boolean)
   */
  public CC2 flowY() {
    cc = cc.flowY();

    return this;
  }

  /**
   * Same functionality as {@link #growX()} and {@link #growY()}.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #growX()
   * @see #growY()
   */
  public CC2 grow() {
    cc = cc.grow();

    return this;
  }

  /**
   * Same functionality as calling {@link #setNewline(boolean)} with <code>true</code> only this
   * method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setNewline(boolean)
   */
  public CC2 newline() {
    cc = cc.newline();

    return this;
  }

  /**
   * Same functionality as {@link #setNewlineGapSize(BoundSize)} only this method returns
   * <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param gapSize The gap size that will override the gap size in the row/column constraints if
   *                <code>!= null</code>. E.g. "5px" or "unrel". If <code>null</code> or
   *                <code>""</code> the newline size will be set to the default size and turned on.
   *                This is different compared to {@link #setNewlineGapSize(BoundSize)}.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setNewlineGapSize(BoundSize)
   */
  public CC2 newline(String gapSize) {
    cc = cc.newline(gapSize);

    return this;
  }

  /**
   * Same functionality as calling {@link #setWrap(boolean)} with <code>true</code> only this method
   * returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setWrap(boolean)
   */
  public CC2 wrap() {
    cc = cc.wrap();

    return this;
  }

  /**
   * Same functionality as {@link #setWrapGapSize(BoundSize)} only this method returns
   * <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param gapSize The gap size that will override the gap size in the row/column constraints if
   *                <code>!= null</code>. E.g. "5px" or "unrel". If <code>null</code> or
   *                <code>""</code> the wrap size will be set to the default size and turned on.
   *                This is different compared to {@link #setWrapGapSize(BoundSize)}.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setWrapGapSize(BoundSize)
   */
  public CC2 wrap(String gapSize) {
    cc = cc.wrap(gapSize);

    return this;
  }

  /**
   * Same functionality as calling {@link #setDockSide(int)} with <code>0</code> only this method
   * returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setDockSide(int)
   */
  public CC2 dockNorth() {
    cc = cc.dockNorth();

    return this;
  }

  /**
   * Same functionality as calling {@link #setDockSide(int)} with <code>1</code> only this method
   * returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setDockSide(int)
   */
  public CC2 dockWest() {
    cc = cc.dockWest();

    return this;
  }

  /**
   * Same functionality as calling {@link #setDockSide(int)} with <code>2</code> only this method
   * returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setDockSide(int)
   */
  public CC2 dockSouth() {
    cc = cc.dockSouth();

    return this;
  }

  /**
   * Same functionality as calling {@link #setDockSide(int)} with <code>3</code> only this method
   * returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setDockSide(int)
   */
  public CC2 dockEast() {
    cc = cc.dockEast();

    return this;
  }

  /**
   * Sets the x-coordinate for the component. This is used to set the x coordinate position to a
   * specific value. The component bounds is still precalculated to the grid cell and this method
   * should be seen as a way to correct the x position.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param x The x position as a UnitValue. E.g. "10" or "40mm" or "container.x+10".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setPos(UnitValue[])
   */
  public CC2 x(String x) {
    cc = cc.x(x);

    return this;
  }

  /**
   * Sets the y-coordinate for the component. This is used to set the y coordinate position to a
   * specific value. The component bounds is still precalculated to the grid cell and this method
   * should be seen as a way to correct the y position.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param y The y position as a UnitValue. E.g. "10" or "40mm" or "container.x+10".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setPos(UnitValue[])
   */
  public CC2 y(String y) {
    cc = cc.y(y);

    return this;
  }

  /**
   * Sets the x2-coordinate for the component (right side). This is used to set the x2 coordinate
   * position to a specific value. The component bounds is still precalculated to the grid cell and
   * this method should be seen as a way to correct the x position.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param x2 The x2 side's position as a UnitValue. E.g. "10" or "40mm" or "container.x2 - 10".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setPos(UnitValue[])
   */
  public CC2 x2(String x2) {
    cc = cc.x2(x2);

    return this;
  }

  /**
   * Sets the y2-coordinate for the component (bottom side). This is used to set the y2 coordinate
   * position to a specific value. The component bounds is still precalculated to the grid cell and
   * this method should be seen as a way to correct the y position.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param y2 The y2 side's position as a UnitValue. E.g. "10" or "40mm" or "container.x2 - 10".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setPos(UnitValue[])
   */
  public CC2 y2(String y2) {
    cc = cc.y2(y2);

    return this;
  }

  /**
   * Same functionality as {@link #x(String x)} and {@link #y(String y)} together.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param x The x position as a UnitValue. E.g. "10" or "40mm" or "container.x+10".
   * @param y The y position as a UnitValue. E.g. "10" or "40mm" or "container.x+10".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setPos(UnitValue[])
   */
  public CC2 pos(String x, String y) {
    cc = cc.pos(x, y);

    return this;
  }

  /**
   * Same functionality as {@link #x(String x)}, {@link #y(String y)}, {@link #y2(String y)} and
   * {@link #y2(String y)} together.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param x  The x position as a UnitValue. E.g. "10" or "40mm" or "container.x+10".
   * @param y  The y position as a UnitValue. E.g. "10" or "40mm" or "container.x+10".
   * @param x2 The x2 side's position as a UnitValue. E.g. "10" or "40mm" or "container.x2 - 10".
   * @param y2 The y2 side's position as a UnitValue. E.g. "10" or "40mm" or "container.x2 - 10".
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setPos(UnitValue[])
   */
  public CC2 pos(String x, String y, String x2, String y2) {
    cc = cc.pos(x, y, x2, y2);

    return this;
  }

  /**
   * Same functionality as {@link #setPadding(UnitValue[])} but the unit values as absolute pixels.
   * This method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param top    The top padding that will be added to the y coordinate at the last stage in the
   *               layout.
   * @param left   The top padding that will be added to the x coordinate at the last stage in the
   *               layout.
   * @param bottom The top padding that will be added to the y2 coordinate at the last stage in the
   *               layout.
   * @param right  The top padding that will be added to the x2 coordinate at the last stage in the
   *               layout.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setTag(String)
   */
  public CC2 pad(int top, int left, int bottom, int right) {
    cc = cc.pad(top, left, bottom, right);

    return this;
  }

  /**
   * Same functionality as <code>setPadding(ConstraintParser.parseInsets(pad, false))}</code> only
   * this method returns <code>this</code> for chaining multiple calls.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param pad The string to parse. E.g. "10 10 10 10" or "20". If less than 4 groups the last will
   *            be used for the missing.
   * @return <code>this</code> so it is possible to chain calls. E.g. <code>new
   *     ComponentConstraint().noGrid().gap().fill()</code>.
   * @see #setTag(String)
   */
  public CC2 pad(String pad) {
    cc = cc.pad(pad);

    return this;
  }

  /**
   * Returns the horizontal dimension constraint for this component constraint. It has constraints
   * for the horizontal size and grow/shrink priorities and weights.
   * <p>
   * Note! If any changes is to be made it must be made direct when the object is returned. It is
   * not allowed to save the constraint for later use.
   *
   * @return The current dimension constraint. Never <code>null</code>.
   */
  public DimConstraint getHorizontal() {
    return cc.getHorizontal();
  }

  /**
   * Sets the horizontal dimension constraint for this component constraint. It has constraints for
   * the horizontal size and grow/shrink priorities and weights.
   *
   * @param h The new dimension constraint. If <code>null</code> it will be reset to <code>new
   *          DimConstraint();</code>
   */
  public void setHorizontal(DimConstraint h) {
    cc.setHorizontal(h);
  }

  /**
   * Returns the vertical dimension constraint for this component constraint. It has constraints for
   * the vertical size and grow/shrink priorities and weights.
   * <p>
   * Note! If any changes is to be made it must be made direct when the object is returned. It is
   * not allowed to save the constraint for later use.
   *
   * @return The current dimension constraint. Never <code>null</code>.
   */
  public DimConstraint getVertical() {
    return cc.getVertical();
  }

  /**
   * Sets the vertical dimension constraint for this component constraint. It has constraints for
   * the vertical size and grow/shrink priorities and weights.
   *
   * @param v The new dimension constraint. If <code>null</code> it will be reset to <code>new
   *          DimConstraint();</code>
   */
  public void setVertical(DimConstraint v) {
    cc.setVertical(v);
  }

  /**
   * Returns the vertical or horizontal dim constraint.
   * <p>
   * Note! If any changes is to be made it must be made direct when the object is returned. It is
   * not allowed to save the constraint for later use.
   *
   * @param isHor If the horizontal constraint should be returned.
   * @return The dim constraint. Never <code>null</code>.
   */
  public DimConstraint getDimConstraint(boolean isHor) {
    return cc.getDimConstraint(isHor);
  }

  /**
   * Returns the absolute positioning of one or more of the edges. This will be applied last in the
   * layout cycle and will not affect the flow or grid positions. The positioning is relative to the
   * parent and can not (as padding) be used to adjust the edges relative to the old value. May be
   * <code>null</code> and elements may be <code>null</code>.
   * <code>null</code> value(s) for the x2 and y2 will be interpreted as to keep the preferred size
   * and thus the x1 and x2 will just absolutely positions the component.
   * <p>
   * slightly.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value as a new array, free to modify.
   */
  public UnitValue[] getPos() {
    return cc.getPos();
  }

  /**
   * Sets absolute positioning of one or more of the edges. This will be applied last in the layout
   * cycle and will not affect the flow or grid positions. The positioning is relative to the parent
   * and can not (as padding) be used to adjust the edges relative to the old value. May be
   * <code>null</code> and elements may be <code>null</code>.
   * <code>null</code> value(s) for the x2 and y2 will be interpreted as to keep the preferred size
   * and thus the x1 and x2 will just absolutely positions the component. For a more thorough
   * explanation of what this constraint does see the white paper or cheat Sheet at
   * www.migcomponents.com.
   *
   * @param pos <code>UnitValue[] {x, y, x2, y2}</code>. Must be <code>null</code> or of length 4.
   *            Elements can be <code>null</code>.
   */
  public void setPos(UnitValue[] pos) {
    cc.setPos(pos);
  }

  /**
   * Returns if the absolute <code>pos</code> value should be corrections to the component that is
   * in a normal cell. If <code>false</code> the value of <code>pos</code> is truly absolute in that
   * it will not affect the grid or have a default bounds in the grid.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   * @see #getPos()
   */
  public boolean isBoundsInGrid() {
    return cc.isBoundsInGrid();
  }

  /**
   * Returns the absolute cell position in the grid or <code>-1</code> if cell positioning is not
   * used.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   */
  public int getCellX() {
    return cc.getCellX();
  }

  /**
   * Set an absolute cell x-position in the grid. If &gt;= 0 this point points to the absolute cell
   * that this constaint's component should occupy. If there's already a component in that cell they
   * will split the cell. The flow will then continue after this cell.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param x The x-position or <code>-1</code> to disable cell positioning.
   */
  public void setCellX(int x) {
    cc.setCellX(x);
  }

  /**
   * Returns the absolute cell position in the grid or <code>-1</code> if cell positioning is not
   * used.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   */
  public int getCellY() {
    return cc.getCellY();
  }

  /**
   * Set an absolute cell x-position in the grid. If &gt;= 0 this point points to the absolute cell
   * that this constaint's component should occupy. If there's already a component in that cell they
   * will split the cell. The flow will then continue after this cell.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param y The y-position or <code>-1</code> to disable cell positioning.
   */
  public void setCellY(int y) {
    cc.setCellY(y);
  }

  /**
   * Sets the docking side. -1 means no docking.<br> Valid sides are: <code> north = 0, west = 1,
   * south = 2, east = 3</code>.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current side.
   */
  public int getDockSide() {
    return cc.getDockSide();
  }

  /**
   * Sets the docking side. -1 means no docking.<br> Valid sides are: <code> north = 0, west = 1,
   * south = 2, east = 3</code>.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param side -1 or 0-3.
   */
  public void setDockSide(int side) {
    cc.setDockSide(side);
  }

  /**
   * Returns if this component should have its bounds handled by an external source and not this
   * layout manager.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   */
  public boolean isExternal() {
    return cc.isExternal();
  }

  /**
   * If this boolean is true this component is not handled in any way by the layout manager and the
   * component can have its bounds set by an external handler which is normally by the use of some
   * <code>component.setBounds(x, y, width, height)</code> directly (for Swing).
   * <p>
   * The bounds <b>will not</b> affect the minimum and preferred size of the container.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param b <code>true</code> means that the bounds are not changed.
   */
  public void setExternal(boolean b) {
    cc.setExternal(b);
  }

  /**
   * Returns if the flow in the <b>cell</b> is in the horizontal dimension. Vertical if
   * <code>false</code>. Only the first component is a cell can set the flow.
   * <p>
   * If <code>null</code> the flow direction is inherited by from the {@link LC}.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   */
  public Boolean getFlowX() {
    return cc.getFlowX();
  }

  /**
   * Sets if the flow in the <b>cell</b> is in the horizontal dimension. Vertical if
   * <code>false</code>. Only the first component is a cell can set the flow.
   * <p>
   * If <code>null</code> the flow direction is inherited by from the {@link LC}.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param b <code>Boolean.TRUE</code> means horizontal flow in the cell.
   */
  public void setFlowX(Boolean b) {
    cc.setFlowX(b);
  }

  /**
   * Sets how a component that is hidden (not visible) should be treated by default. For a more
   * thorough explanation of what this constraint does see the white paper or cheat Sheet at
   * www.migcomponents.com.
   *
   * @return The mode:<br> 0 == Normal. Bounds will be calculated as if the component was
   *     visible.<br> 1 == If hidden the size will be 0, 0 but the gaps remain.<br> 2 == If hidden
   *     the size will be 0, 0 and gaps set to zero.<br> 3 == If hidden the component will be
   *     disregarded completely and not take up a cell in the grid..
   */
  public int getHideMode() {
    return cc.getHideMode();
  }

  /**
   * Sets how a component that is hidden (not visible) should be treated by default.
   *
   * @param mode The mode:<br> 0 == Normal. Bounds will be calculated as if the component was
   *             visible.<br> 1 == If hidden the size will be 0, 0 but the gaps remain.<br> 2 == If
   *             hidden the size will be 0, 0 and gaps set to zero.<br> 3 == If hidden the component
   *             will be disregarded completely and not take up a cell in the grid..
   */
  public void setHideMode(int mode) {
    cc.setHideMode(mode);
  }

  /**
   * Returns the id used to reference this component in some constraints.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The id or <code>null</code>. May consist of a groupID and an componentID which are
   *     separated by a dot: ".". E.g. "grp1.id1". The dot should never be first or last if
   *     present.
   */
  public String getId() {
    return cc.getId();
  }

  /**
   * Sets the id used to reference this component in some constraints.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param id The id or <code>null</code>. May consist of a groupID and an componentID which are
   *           separated by a dot: ".". E.g. "grp1.id1". The dot should never be first or last if
   *           present.
   */
  public void setId(String id) {
    cc.setId(id);
  }

  /**
   * Returns the absolute resizing in the last stage of the layout cycle. May be <code>null</code>
   * and elements may be <code>null</code>.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value. <code>null</code> or of length 4.
   */
  public UnitValue[] getPadding() {
    return cc.getPadding();
  }

  /**
   * Sets the absolute resizing in the last stage of the layout cycle. These values are added to the
   * edges and can thus for instance be used to grow or reduce the size or move the component an
   * absolute number of pixels. May be <code>null</code> and elements may be <code>null</code>.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param sides top, left, bottom right. Must be <code>null</code> or of length 4.
   */
  public void setPadding(UnitValue[] sides) {
    cc.setPadding(sides);
  }

  /**
   * Returns the visual padding used when laying out this Component. May be <code>null</code> and
   * elements may be <code>null</code>.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value. <code>null</code> or of length 4.
   */
  public UnitValue[] getVisualPadding() {
    return cc.getVisualPadding();
  }

  /**
   * Sets the visual padding used when laying out this Component.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param sides top, left, bottom right. Must be <code>null</code> or of length 4.
   */
  public void setVisualPadding(UnitValue[] sides) {
    cc.setVisualPadding(sides);
  }

  /**
   * Returns how many cells in the grid that should be skipped <b>before</b> the component that this
   * constraint belongs to.
   * <p>
   * Note that only the first component will be checked for this property.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value. 0 if no skip.
   */
  public int getSkip() {
    return cc.getSkip();
  }

  /**
   * Sets how many cells in the grid that should be skipped <b>before</b> the component that this
   * constraint belongs to.
   * <p>
   * Note that only the first component will be checked for this property.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param cells How many cells in the grid that should be skipped <b>before</b> the component that
   *              this constraint belongs to
   */
  public void setSkip(int cells) {
    cc.setSkip(cells);
  }

  /**
   * Returns the number of cells the cell that this constraint's component will span in the
   * indicated dimension. <code>1</code> is default and means that it only spans the current cell.
   * <code>LayoutUtil.INF</code> is used to indicate a span to the end of the column/row.
   * <p>
   * Note that only the first component will be checked for this property.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   */
  public int getSpanX() {
    return cc.getSpanX();
  }

  /**
   * Sets the number of cells the cell that this constraint's component will span in the indicated
   * dimension. <code>1</code> is default and means that it only spans the current cell.
   * <code>LayoutUtil.INF</code> is used to indicate a span to the end of the column/row.
   * <p>
   * Note that only the first component will be checked for this property.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param cells The number of cells to span (i.e. merge).
   */
  public void setSpanX(int cells) {
    cc.setSpanX(cells);
  }

  /**
   * Returns the number of cells the cell that this constraint's component will span in the
   * indicated dimension. <code>1</code> is default and means that it only spans the current cell.
   * <code>LayoutUtil.INF</code> is used to indicate a span to the end of the column/row.
   * <p>
   * Note that only the first component will be checked for this property.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   */
  public int getSpanY() {
    return cc.getSpanY();
  }

  /**
   * Sets the number of cells the cell that this constraint's component will span in the indicated
   * dimension. <code>1</code> is default and means that it only spans the current cell.
   * <code>LayoutUtil.INF</code> is used to indicate a span to the end of the column/row.
   * <p>
   * Note that only the first component will be checked for this property.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param cells The number of cells to span (i.e. merge).
   */
  public void setSpanY(int cells) {
    cc.setSpanY(cells);
  }

  /**
   * "pushx" indicates that the column that this component is in (this first if the component spans)
   * should default to growing. If any other column has been set to grow this push value on the
   * component does nothing as the column's explicit grow weight will take precedence. Push is
   * normally used when the grid has not been defined in the layout.
   * <p>
   * If multiple components in a column has push weights set the largest one will be used for the
   * column.
   *
   * @return The current push value. Default is <code>null</code>.
   */
  public Float getPushX() {
    return cc.getPushX();
  }

  /**
   * "pushx" indicates that the column that this component is in (this first if the component spans)
   * should default to growing. If any other column has been set to grow this push value on the
   * component does nothing as the column's explicit grow weight will take precedence. Push is
   * normally used when the grid has not been defined in the layout.
   * <p>
   * If multiple components in a column has push weights set the largest one will be used for the
   * column.
   *
   * @param weight The new push value. Default is <code>null</code>.
   */
  public void setPushX(Float weight) {
    cc.setPushX(weight);
  }

  /**
   * "pushx" indicates that the row that this component is in (this first if the component spans)
   * should default to growing. If any other row has been set to grow this push value on the
   * component does nothing as the row's explicit grow weight will take precedence. Push is normally
   * used when the grid has not been defined in the layout.
   * <p>
   * If multiple components in a row has push weights set the largest one will be used for the row.
   *
   * @return The current push value. Default is <code>null</code>.
   */
  public Float getPushY() {
    return cc.getPushY();
  }

  /**
   * "pushx" indicates that the row that this component is in (this first if the component spans)
   * should default to growing. If any other row has been set to grow this push value on the
   * component does nothing as the row's explicit grow weight will take precedence. Push is normally
   * used when the grid has not been defined in the layout.
   * <p>
   * If multiple components in a row has push weights set the largest one will be used for the row.
   *
   * @param weight The new push value. Default is <code>null</code>.
   */
  public void setPushY(Float weight) {
    cc.setPushY(weight);
  }

  /**
   * Returns in how many parts the current cell (that this constraint's component will be in) should
   * be split in. If for instance it is split in two, the next component will also share the same
   * cell. Note that the cell can also span a number of cells, which means that you can for instance
   * span three cells and split that big cell for two components. Split can be set to a very high
   * value to make all components in the same row/column share the same cell (e.g.
   * <code>LayoutUtil.INF</code>).
   * <p>
   * Note that only the first component will be checked for this property.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   */
  public int getSplit() {
    return cc.getSplit();
  }

  /**
   * Sets in how many parts the current cell (that this constraint's component will be in) should be
   * split in. If for instance it is split in two, the next component will also share the same cell.
   * Note that the cell can also span a number of cells, which means that you can for instance span
   * three cells and split that big cell for two components. Split can be set to a very high value
   * to make all components in the same row/column share the same cell (e.g.
   * <code>LayoutUtil.INF</code>).
   * <p>
   * Note that only the first component will be checked for this property.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param parts The number of parts (i.e. component slots) the cell should be divided into.
   */
  public void setSplit(int parts) {
    cc.setSplit(parts);
  }

  /**
   * Tags the component with metadata. Currently only used to tag buttons with for instance "cancel"
   * or "ok" to make them show up in the correct order depending on platform. For a more thorough
   * explanation of what this constraint does see the white paper or cheat Sheet at
   * www.migcomponents.com.
   *
   * @return The current value. May be <code>null</code>.
   */
  public String getTag() {
    return cc.getTag();
  }

  /**
   * Optional tag that gives more context to this constraint's component. It is for instance used to
   * tag buttons in a button bar with the button type such as "ok", "help" or "cancel".
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param tag The new tag. May be <code>null</code>.
   */
  public void setTag(String tag) {
    cc.setTag(tag);
  }

  /**
   * Returns if the flow should wrap to the next line/column <b>after</b> the component that this
   * constraint belongs to.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   */
  public boolean isWrap() {
    return cc.isWrap();
  }

  /**
   * Sets if the flow should wrap to the next line/column <b>after</b> the component that this
   * constraint belongs to.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param b <code>true</code> means wrap after.
   */
  public void setWrap(boolean b) {
    cc.setWrap(b);
  }

  /**
   * Returns the wrap size if it is a custom size. If wrap was set to true with {@link
   * #setWrap(boolean)} then this method will return <code>null</code> since that means that the gap
   * size should be the default one as defined in the rows spec.
   *
   * @return The custom gap size. NOTE! Will return <code>null</code> for both no wrap <b>and</b>
   *     default wrap.
   * @see #isWrap()
   * @see #setWrap(boolean)
   * @since 2.4.2
   */
  public BoundSize getWrapGapSize() {
    return cc.getWrapGapSize();
  }

  /**
   * Set the wrap size and turns wrap on if <code>!= null</code>.
   *
   * @param s The custom gap size. NOTE! <code>null</code> will not turn on or off wrap, it will
   *          only set the wrap gap size to "default". A non-null value will turn on wrap though.
   * @see #isWrap()
   * @see #setWrap(boolean)
   * @since 2.4.2
   */
  public void setWrapGapSize(BoundSize s) {
    cc.setWrapGapSize(s);
  }

  /**
   * Returns if the flow should wrap to the next line/column <b>before</b> the component that this
   * constraint belongs to.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @return The current value.
   */
  public boolean isNewline() {
    return cc.isNewline();
  }

  /**
   * Sets if the flow should wrap to the next line/column <b>before</b> the component that this
   * constraint belongs to.
   * <p>
   * For a more thorough explanation of what this constraint does see the white paper or cheat Sheet
   * at www.migcomponents.com.
   *
   * @param b <code>true</code> means wrap before.
   */
  public void setNewline(boolean b) {
    cc.setNewline(b);
  }

  /**
   * Returns the newline size if it is a custom size. If newline was set to true with {@link
   * #setNewline(boolean)} then this method will return <code>null</code> since that means that the
   * gap size should be the default one as defined in the rows spec.
   *
   * @return The custom gap size. NOTE! Will return <code>null</code> for both no newline <b>and</b>
   *     default newline.
   * @see #isNewline()
   * @see #setNewline(boolean)
   * @since 2.4.2
   */
  public BoundSize getNewlineGapSize() {
    return cc.getNewlineGapSize();
  }

  /**
   * Set the newline size and turns newline on if <code>!= null</code>.
   *
   * @param s The custom gap size. NOTE! <code>null</code> will not turn on or off newline, it will
   *          only set the newline gap size to "default". A non-null value will turn on newline
   *          though.
   * @see #isNewline()
   * @see #setNewline(boolean)
   * @since 2.4.2
   */
  public void setNewlineGapSize(BoundSize s) {
    cc.setNewlineGapSize(s);
  }

  /**
   * Returns the animation spec. Default is a spec where animation is off (prio 0).
   *
   * @return Never null.
   */
  public AnimSpec getAnimSpec() {
    return cc.getAnimSpec();
  }

  public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
    cc.readExternal(in);
  }

  public void writeExternal(ObjectOutput out) throws IOException {
    cc.writeExternal(out);
  }
}
