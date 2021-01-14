# _________________________________________________________________________________________
# -----------------------------------------------------------------------------------------
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.ticker as tck
import pandas as pd
from matplotlib.ticker import FuncFormatter
# ------------------------------------------Format-----------------------------------------
mpl.use('pdf')
# ------------------------------------------------------------------------------------------

# ----------------------Function for making integers the numbers in axis--------------------
def integer_formatter(x, pos):
    if x.is_integer():
        return str(int(x))
    else:
        return str(x)


integer_maker = FuncFormatter(integer_formatter)
# ------------------------------------------------------------------------------------------

# -----------------------Function for plotting 'pi' symbol in axes--------------------------
fractions = 'off'


def multiple_formatter(denominator=2, number=np.pi, latex='\mathrm{\pi}'):
    def gcd(a, b):
        while b:
            a, b = b, a % b
        return a

    def _multiple_formatter(x, pos):
        den = denominator
        num = np.int(np.rint(den * x / number))
        com = gcd(num, den)
        (num, den) = (int(num / com), int(den / com))

        if fractions == "on":
            if den == 1:
                if num == 0:
                    return r'$0$'
                if num == 1:
                    return r'$%s$' % latex
                elif num == -1:
                    return r'$-%s$' % latex
                else:
                    return r'$%s%s$' % (num, latex)
            else:
                if num == 1:
                    return r'$\frac{%s}{%s}$' % (latex, den)
                elif num == -1:
                    return r'$\frac{-%s}{%s}$' % (latex, den)
                else:
                    return r'$\frac{%s%s}{%s}$' % (num, latex, den)

        elif fractions == "off":

            if den == 1:
                if num == 0:
                    return r'$0$'
                if num == 1:
                    return r'$%s$' % latex
                elif num == -1:
                    return r'$-%s$' % latex
                else:
                    return r'$%s%s$' % (num, latex)
            else:
                if num == 1:
                    return r'${%s}{%s}$' % (1 / den, latex)
                elif num == -1:
                    return r'${-%s}{%s}$' % (1 / den, latex)
                else:
                    return r'${%s}{%s}$' % (num / den, latex)
    return _multiple_formatter


class Multiple:
    def __init__(self, denominator=2, number=np.pi, latex='\mathrm{\pi}'):
        self.denominator = denominator
        self.number = number
        self.latex = latex

    def locator(self):
        return plt.MultipleLocator(self.number / self.denominator)

    def formatter(self):
        return plt.FuncFormatter(multiple_formatter(self.denominator,
                                                    self.number, self.latex))
# -----------------------------------------------------------------------------------------


# ------------------------- Color lightness and Dimmness function -------------------------
def lighten_color(color, amount=0.5):

    import matplotlib.colors as mc
    import colorsys
    try:
        c = mc.cnames[color]
    except:
        c = color
    c = colorsys.rgb_to_hls(*mc.to_rgb(c))
    return colorsys.hls_to_rgb(c[0], 1 - amount * (1 - c[1]), c[2])
# -----------------------------------------------------------------------------------------


# --------------------Function for extracting unique elements in an array------------------
# function to get unique values
def unique(list1):
    x = np.array(list1)
    print(np.unique(x))
# -----------------------------------------------------------------------------------------


# -------------------------Settings Tex and Math Fonts and Size of Axes--------------------
plt.rcParams['text.usetex'] = True
plt.rc('font', family='Latin Modern', serif='Latin Modern Math')
plt.rcParams.update({'mathtext.fontset': 'custom',
                     'mathtext.rm': 'Latin Modern Math'})
plt.rc('xtick', labelsize=25)
plt.rc('ytick', labelsize=25)
plt.rc('axes', labelsize=12)
plt.rc('axes', linewidth=4)
fontsize_global = 35

plt.rcParams['axes.unicode_minus'] = False
# -----------------------------------------------------------------------------------------

# ------------------------------Size and Shape of the Picture------------------------------
widescreen = 1.618
rectangular = 1

width = 10
height = width / widescreen
# height = 13
# -----------------------------------------------------------------------------------------

# -------------------------------------Create Subplots-------------------------------------
fig, (ax1, ax2) = plt.subplots(2)
fig.subplots_adjust(left=.2, bottom=.125, right=.77, top=.89)
grid = plt.GridSpec(2, 2, width_ratios=[1, 3], height_ratios=[2, 0], wspace=0.1, hspace=0.2)

ax1 = plt.subplot(grid[0, 1])
ax2 = plt.subplot(grid[0, 0])
# -----------------------------------------------------------------------------------------
spacer_data = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/spacer.txt'
spacer = pd.read_csv(spacer_data, sep='\s+', header=None)

dis1 = spacer.iloc[0, 0]
disN = spacer.iloc[0, 1]
n_layer = int(spacer.iloc[0, 2])
edgeways_width_left = spacer.iloc[0, 3]
edgeways_width_right = spacer.iloc[0, 4]
length = spacer.iloc[0, 5]
n_mater = spacer.iloc[0, 6]
swift = spacer.iloc[0, 7]

layers_data = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/layers.txt'
layers = pd.read_csv(layers_data, sep='\s+', header=None)

DIS = []

for i in range(0, n_layer, 1):
    distance = np.array([layers.iloc[i]])
    DIS.append(distance)

matter_data = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/matter.txt'
material = pd.read_csv(matter_data, sep='\s+', header=None)
# ----------------------------------------------PLOT 1-------------------------------------

#                      -----------------------Set Grid----------------------
ax1.grid(False)
Filter = "On"  # It is either "On" or "Off".
#                      -----------------------------------------------------

#                      ------------------------X_Axis-----------------------
left_x = - (edgeways_width_left + swift)
right_x = (length + edgeways_width_right - swift)

ax1.set_xlabel(r'$z / a$', fontname="Latin Modern Math", fontsize=fontsize_global,
               fontweight='normal')
for tick in ax1.get_xticklabels():
    tick.set_fontname("Latin Modern Math")
    # tick.set_horizontalalignment('center')
ax1.xaxis.set_label_coords(0.51, -0.18)
ax1.set_xlim(left_x, right_x)
#                      -----------------------------------------------------

#                      ------------------------Y_Axis-----------------------

ax1.set_ylabel(r'$|\mathrm{u}(z)|^2$', fontname="Latin Modern Math", fontsize=fontsize_global,
               fontweight='normal')
for tick in ax1.get_yticklabels():
    tick.set_fontname("Latin Modern Math")
    # tick.set_horizontalalignment('center')
ax1.yaxis.set_label_coords(1.37, 0.51)

ya = 0
yb = 65

ax1.set_ylim(ya, yb)
#                      -----------------------------------------------------

#                      ------------------Relative Settings------------------
ax1.yaxis.tick_right()

ax1.tick_params(axis='y', direction='out', length=7, width=1, color='black', pad=10,
                grid_color='black', grid_alpha=0.3, grid_linewidth=0.5)

ax1.tick_params(axis='x', direction='out', length=7, width=1, color='black', pad=10,
                grid_color='black', grid_alpha=0.3, grid_linewidth=0.5)
#                      -----------------------------------------------------

#                      -------------------Auto Set Axes---------------------

#                          ---------------------Notes-------------------
#
#                          If a pi exist in one of the axes then enable
#                          the Muplitple_formater().
#
#                          ---------------------------------------------

#                              ---------------Pi Nugget--------------
# ax1.xaxis.set_major_formatter(plt.FuncFormatter(multiple_formatter()))
#                              --------------------------------------


ax1.xaxis.set_major_locator(tck.MultipleLocator(5))

# ax1.yaxis.set_major_formatter(integer_maker)
ax1.yaxis.set_major_locator(tck.MultipleLocator(20))

#                      -----------------------------------------------------

# -----------------------------------------------------------------------------------------

# ----------------------------------------------PLOT 2-------------------------------------

#                      -----------------------Set Grid----------------------
ax2.grid(False)
#                      -----------------------------------------------------

#                      ------------------------X_Axis-----------------------
ax2.set_xlabel(r'$\mathcal{T}$', fontname="Latin Modern Math", fontsize=fontsize_global,
               fontweight='normal', rotation=0)
for tick in ax2.get_xticklabels():
    tick.set_fontname("Latin Modern Math")
ax2.xaxis.set_label_coords(0.51, -0.18)
ax2.set_xlim(0, 1)
#                      -----------------------------------------------------

#                      ------------------------Y_Axis-----------------------
ax2.set_ylabel(r'$\omega a / v_{0}$', fontname="Latin Modern Math", fontsize=fontsize_global,
               fontweight='normal', rotation=90)
for tick in ax2.get_yticklabels():
    tick.set_fontname("Latin Modern Math")
ax2.yaxis.set_label_coords(-0.7, 0.5)
ax2.set_ylim(0, 1)
#                      -----------------------------------------------------

#                      ------------------Relative Settings------------------
ax2.tick_params(axis='x', direction='out', length=7, width=1, color='black', pad=10,
                grid_color='black', grid_alpha=0.3, grid_linewidth=0.5)


ax2.tick_params(axis='y', direction='out', length=7, width=1, color='black', pad=10,
                grid_color='black', grid_alpha=0.3, grid_linewidth=0.5)
#                      -----------------------------------------------------

#                      -------------------Auto Set Axes---------------------

#                          ---------------------Notes-------------------
#
#                          If a pi exist in one of the axes then enable
#                          the Muplitple_formater().
#
#                          ---------------------------------------------

#                              ---------------Pi Nugget--------------
# ax2.xaxis.set_major_formatter(plt.FuncFormatter(multiple_formatter()))
#                              --------------------------------------
ax2.xaxis.set_major_locator(tck.MultipleLocator(1))
ax2.xaxis.set_major_formatter(integer_maker)
ax2.yaxis.set_major_locator(tck.MultipleLocator(0.5))
#                      -----------------------------------------------------

# -----------------------------------------------------------------------------------------

# --------------------------------Input Data for Plotting----------------------------------
location_of_input_file_PREVIOUS = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data.txt'

data_from_below = pd.read_csv(location_of_input_file_PREVIOUS, sep='\s+', header=None)

location_of_input_file = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_band_d.txt'

data = pd.read_csv(location_of_input_file, sep='\s+', header=None)

location_of_input_file_field_d_INNER_X = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_INNER_X.txt'

data_field_d_INNER_X = pd.read_csv(location_of_input_file_field_d_INNER_X, sep='\s+', header=None)

location_of_input_file_field_d_INNER_Y = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_INNER_Y.txt'

data_field_d_INNER_Y = pd.read_csv(location_of_input_file_field_d_INNER_Y, sep='\s+', header=None)

location_of_rtq_data_d_IN_ARGUMENT = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_d_IN_ARGUMENT.txt'

argument_in = pd.read_csv(location_of_rtq_data_d_IN_ARGUMENT, sep='\s+', header=None)

location_of_input_file_field_d_LRB = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_LRB.txt'

data_field_d_LRB = pd.read_csv(location_of_input_file_field_d_LRB, sep='\s+', header=None)

location_of_input_file_field_d_L = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_L.txt'

data_field_d_L = pd.read_csv(location_of_input_file_field_d_L, sep='\s+', header=None)

location_of_input_file_field_d_R = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field_d_R.txt'

data_field_d_R = pd.read_csv(location_of_input_file_field_d_R, sep='\s+', header=None)

N = len(data_field_d_INNER_X.columns)

M = len(data_field_d_INNER_Y.columns)

kmax = len(data.index)
# -----------------------------------------------------------------------------------------

# -------------------------------Set Options for Data Points-------------------------------
rainbow = ['black', 'darkred', 'royalblue', 'green', 'teal',
           'darkgoldenrod', 'cyan', 'magenta', 'lavender', 'chocolate',
           'black', 'darkred', 'royalblue', 'green', 'teal',
           'darkgoldenrod', 'cyan', 'magenta', 'lavender', 'chocolate',
           'black', 'darkred', 'royalblue', 'green', 'teal',
           'darkgoldenrod', 'cyan', 'magenta', 'lavender', 'chocolate',
           'black']

color1 = 'black'
color2 = lighten_color('red', 1.1)
color3 = 'royalblue'
color4 = 'green'
color5 = 'teal'
color6 = 'darkgoldenrod'
color7 = 'cyan'
color8 = 'magenta'
color9 = 'lavender'
color10 = 'chocolate'

plot_line_size = 3
scatter_size = plot_line_size
line_size = plot_line_size - 1
transparency = 1
#                      ---------------------Notes--------------------
#
#                      Transparency is an option only for pdf outputs
#                      and not for eps outputs.
#
#                      ----------------------------------------------

# -----------------------------------------------------------------------------------------

# -----------------------------------Plotting the Plots------------------------------------

#                      ---------------------Scatter Plot--------------------
x = data.iloc[:, 0]
y = data.iloc[:, 1]

ax2.plot(y, x, c=color2, alpha=1.0, linewidth=line_size)

x1_f = data_field_d_LRB.iloc[:, 0]
x2_f = data_field_d_LRB.iloc[:, 1]
x3_f = data_field_d_LRB.iloc[:, 2]
x4_f = data_field_d_LRB.iloc[:, 3]

y1_f = data_field_d_LRB.iloc[:, 4]
y2_f = data_field_d_LRB.iloc[:, 5]
y3_f = data_field_d_LRB.iloc[:, 6]
y4_f = data_field_d_LRB.iloc[:, 7]

ax1.plot(x1_f, y1_f, c=color1, alpha=1.0, linewidth=line_size)
ax1.plot(x2_f, y2_f, c=color1, alpha=1.0, linewidth=line_size)
ax1.plot(x3_f, y3_f, c=color1, alpha=1.0, linewidth=line_size)
ax1.plot(x4_f, y4_f, c=color1, alpha=1.0, linewidth=line_size)

x_below = data_from_below.iloc[:, 3]
y_below = data_from_below.iloc[:, 0]
ax2.plot(x_below, y_below, c=color1, alpha=0.0, linewidth=line_size)

for i in range(0, N - 1, 1):
    x_f = data_field_d_INNER_X.iloc[:, i]
    y_f = data_field_d_INNER_Y.iloc[:, i]
    fi_y = argument_in.iloc[:, i]
    ax1.plot(x_f, y_f, c=color1, alpha=1.0, linewidth=line_size)    # Abs(Field) ** (2) or Abs(Field) or Real(Field)
    ax1.plot(x_f, fi_y, c=color3, alpha=0.0, linewidth=line_size)   # Phase(Field) or Imag(Field)
# -----------------------------------------------------------------------------------------
mpl.rcParams['font.family'] = "Latin Modern"
mpl.rcParams['font.serif'] = "Latin Modern Math"
#                      ---------------------Second Run----------------------
rectangle = plt.Rectangle((0 - swift, 0),
                          -edgeways_width_left, yb,
                          fc='grey',
                          alpha=0.2, label='Ενεργειακό\nΧάσμα')
ax1.add_patch(rectangle)

rectangle = plt.Rectangle((length - swift, 0),
                          -DIS[n_layer - 1], yb,
                          fc='grey',
                          alpha=0.2, label='Ενεργειακό\nΧάσμα')
ax1.add_patch(rectangle)

rectangle = plt.Rectangle((length - swift, 0),
                          edgeways_width_right, yb,
                          fc='grey',
                          alpha=0.2, label='Ενεργειακό\nΧάσμα')
ax1.add_patch(rectangle)

z_start = 0.0 - swift - DIS[1]

for i in range(0, n_layer - 1, 1):

    if (material.iloc[i] == 1).bool():
        auto_color = 'grey'
    elif (material.iloc[i] == 2).bool():
        auto_color = 'black'
    elif (material.iloc[i] == 3).bool():
        auto_color = 'royalblue'
    elif (material.iloc[i] == 4).bool():
        auto_color = 'green'
    elif (material.iloc[i] == 5).bool():
        auto_color = 'teal'
    elif (material.iloc[i] == 6).bool():
        auto_color = 'darkgoldenrod'
    elif (material.iloc[i] == 7).bool():
        auto_color = 'cyan'
    elif (material.iloc[i] == 8).bool():
        auto_color = 'magenta'
    elif (material.iloc[i] == 9).bool():
        auto_color = 'lavender'
    else:
        auto_color = 'pink'

    z_start = z_start + DIS[i - 1]
    z_end = DIS[i]

    rectangle = plt.Rectangle((z_start, 0),
                              z_end, yb,
                              fc=auto_color,
                              alpha=0.2, label='Ενεργειακό\nΧάσμα')
    ax1.add_patch(rectangle)
# ------------------------Printing Plots in a format (pdf, eps, etc.)----------------------
fig.set_size_inches(width, height)
fig.savefig('plot.pdf', transparent=True)
# -----------------------------------------------------------------------------------------

# _________________________________________________________________________________________
# -----------------------------------------------------------------------------------------