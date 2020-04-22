# Fig4d
Figma one-way syncing for Cinema4D  

## Feature Support

### Support Values:
| Value     | Renders correctly | Fully editable in c4d | No loss of data |
|-----------|-------------------|-----------------------|-----------------|
| Yes       | ✅                | ✅                    | ✅              |
| Baked     | ✅                | 🚫                    | ✅              |
| Partial   | 🤷                | 🤷                    | ✅              |
| Lossy     | 🤷                | 🤷                    | 🚫              |
| ???       | 🤷                | 🤷                    | 🤷              |
| Data Only | 🚫                | 🤷                    | ✅              |
| No        | 🚫                | 🚫                    | 🚫              | 

---

### All Layer Types
| Feature          | Support                          |
|------------------|----------------------------------|
| Layer Locked     | Partial                          |
| Layer Visibility | Yes                              |
| Layer Masking    | No                               |
| Transforms       | Yes                              |
| Constraints      | Baked                            |
| Blend Mode       | No                               |
| Opacity          | Yes                              |
| Fill             | [*See fill support*](#Fills)     |
| Stroke           | [*See stroke support*](#Strokes) |
| Effects          | [*See effect support*](#Effects) |
| Export           | No                               |
  
Locked layers are locked in c4d but their children are not.

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

### Slices
Currently not supported.  

---

### Components / Instances
Fully supported, baked down to its contents.

---

### Fills
| Feature             | Support              |
|---------------------|----------------------|
| Stacking Fills      | Yes                  |
| Fill Opacity        | Yes                  |
| Fill Blending Modes | No                   |
| Visibility          | Yes                  |
| Solid Fill          | Yes                  |
| Linear Fill         | No                   |
| Radial Fill         | No                   |
| Angular Fill        | No                   |
| Diamond Fill        | No                   |
| Image Fill          | No                   |
  
Stacking will layer c4d materials over eachother and will not use blending modes.  
Fill layers with visibility turned off will be in the materials pane, but not assigned to the object.

---

### Strokes
| Feature               | Support              |
|-----------------------|----------------------|
| Stacking Strokes      | Yes                  |
| Stroke Opacity        | Yes                  |
| Stroke Blending Modes | No                   |
| Visibility            | Yes                  |
| Solid Stroke          | Yes                  |
| Linear Stroke         | No                   |
| Radial Stroke         | No                   |
| Angular Stroke        | No                   |
| Diamond Stroke        | No                   |
| Image Stroke          | No                   |
| Stroke Width          | Yes                  |
| Stroke Align          | Yes                  |
| Caps                  | ???                  |
| Joins                 | ???                  |
| Dashes                | ???                  |
| Miter Angle           | ???                  |
  
Stacking will layer c4d materials over eachother and will not use blending modes.  
Stroke layers with visibility turned off will be in the materials pane, but not assigned to the object.

---

### Effects
| Feature            | Support              |
|--------------------|----------------------|
| Stacking Effects   | Data Only            |
| Visibility         | Data Only            |
| Drop Shadow        | Data Only            |
| Inner Shadow       | Data Only            |
| Layer Blur         | Data Only            |
| Background Blur    | Data Only            |
  
Effects are applied to the object as a data tag.
