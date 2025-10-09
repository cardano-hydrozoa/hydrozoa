# IntelliJ IDEA Configuration Guide

This document contains useful IntelliJ IDEA configuration tips and tricks for working with Scala projects.

## Table of Contents

- [Live Templates](#live-templates)
  - [Section Separator Template](#section-separator-template)
- [Code Formatting and Indentation](#code-formatting-and-indentation)
  - [Fixing Indentation Issues](#fixing-indentation-issues)
  - [Scalafmt Integration](#scalafmt-integration)
- [ScalaDoc Configuration](#scaladoc-configuration)

## Live Templates

### Section Separator Template

Create a live template to quickly insert section separators in your code.

#### Setup Instructions

1. **Create the Live Template**
   - Go to **Settings → Editor → Live Templates**
   - Click the **+** button → Select **"Live Template"**
   - Configure the template:
     - **Abbreviation**: `sep` (or your preferred trigger)
     - **Description**: "Insert section separator"
     - **Template text**:
       ```
       // ===================================
       // $NAME$
       // ===================================
       ```

2. **Configure Variables**
   - Click **"Define"** and select **"Scala"** (or **"Everywhere"**)
   - Click **"Edit variables"** and set `NAME` with Expression: `complete()`

3. **Usage**
   - Type `sep` and press **Tab**
   - The template will expand and prompt you to enter the section name

## Code Formatting and Indentation

### Fixing Indentation Issues

Solution for IDEA's problematic indentation behavior (especially when pressing Enter inside parentheses).

#### Configuration Steps

1. **Access Code Style Settings**
   - Go to **Settings → Editor → Code Style → Scala**

2. **Set Formatter to IntelliJ (Temporarily)**
   - Select **Formatter = IntelliJ** (not Scalafmt)

3. **Configure Tabs and Indents**
   - Go to the **"Tabs and Indents"** tab
   - Set the following values:
     - **Tab size**: 4
     - **Indent**: 4
     - **Continuation indent**: 4

4. **Configure Wrapping and Braces**
   - Go to the **"Wrapping and Braces"** tab
   - In the **"Method declaration parameters"** group:
     - **Deactivate**: "Align when multiline"
     - **Activate**: "Use normal indent for parameters"

### Scalafmt Integration

After configuring the IntelliJ settings above:

- **Switch back to Scalafmt**: Select **Formatter = Scalafmt** (if you want to run it with **Ctrl+Alt+L**)

#### How It Works

Even though you're using Scalafmt, IDEA will remember the IntelliJ settings for real-time formatting while typing. This gives you:

**Before configuration:**
```scala
class Foo (
          a: Int,
          b: String
)
```

**After configuration:**
```scala
class Foo (
    a: Int,
    b: String
)
```

#### Key Points

- **Real-time formatting**: IDEA uses IntelliJ code-style settings while typing
- **Manual formatting**: Scalafmt settings apply when running formatter manually
- **Settings visibility**: IntelliJ settings are only visible when temporarily switching to IntelliJ formatter mode

## ScalaDoc Configuration

### Additional Space for Leading Asterisk

For better ScalaDoc comment formatting:

1. Go to **Settings → Editor → Code Style → Scala → ScalaDoc** tab
2. **Activate**: "Add additional space for leading asterisk"

#### Result

**Before:**
```scala
/** Before
* some text
  */
```

**After:**
```scala
/** After
 * some text
 */
```
