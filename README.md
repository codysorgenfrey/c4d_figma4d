# Fig4d
Figma one-way syncing for Cinema4D  

## Feature Support

Keys:
* Yes - Fully supported and editable in c4d.
* Baked - Fully supported but not editable in c4d.
* Partial - Supported, but may not look correct.
* Data Only - Values are imported, but not used. There may be incorrect rendering.
* No - Not supported or imported, may be loss of data or incorrect rendering.


### All Layer Types
| Feature          | Support              |
|------------------|----------------------|
| Layer Locked     | No                   |
| Layer Visibility | Yes                  |
| Transforms       | Yes                  |
| Constraints      | Baked                |
| Blend Mode       | No                   |
| Fill             | *See fill support*   |
| Stroke           | *See stroke support* |
| Effects          | *See effect support* |
| Export           | No                   |

---

### Frames / Groups
| Feature          | Support              |
|------------------|----------------------|
| Clip Content     | No                   |
| Corner Radius    | No                   |
| Auto Layout      | Baked                |
| Layout Grids     | No                   |

---

### Vectors
| Type             | Support              |
|------------------|----------------------|
| Pen Tool         | Yes                  |
| Pencil Tool      | Yes                  |
| Line             | Yes                  |
| Arrow            | Yes                  |
| Rectangle        | Baked                |
| Star             | Baked                |
| Ellipse          | Baked                |
| Rectangle        | Baked                |
| Polygon          | Baked                |
| Text             | Baked                |

---

### Text
| Type                 | Support              |
|----------------------|----------------------|
| Font Family          | Partial              |
| Font Weight          | Partial              |
| Font Size            | Partial              |
| Line Height          | No                   |
| Letter Spacing       | Partial              |
| Paragraph Spacing    | No                   |
| Horizontal Alignment | Partial              |
| Vertical Alignment   | No                   |
| Decoration           | No                   |
| Paragraph Indent     | No                   |
| Letter Case          | ???                  |
| Number Position      | No                   |
| Kerning Pairs        | Partial              |
  
Currently C4D only supports one style per text layer.  
Due to rendering discrepancy text may be misaligned. We know this is unacceptable, which is why we also import a vectorized version of every text layer.

---

### Slice
Currently not supported.  

---

### Components / Instances
Fully supported, baked down to it's contents.

---

### Fills
| Feature          | Support              |
|------------------|----------------------|

---

### Strokes
| Feature          | Support              |
|------------------|----------------------|

---

### Effects
| Feature          | Support              |
|------------------|----------------------|
