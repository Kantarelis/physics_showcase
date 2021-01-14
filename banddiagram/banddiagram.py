# _________________________________________________________________________________________
# ------------------------------------Band Diagram Factory---------------------------------

import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.ticker as tck
import pandas as pd
from matplotlib.ticker import FuncFormatter
from matplotlib.patches import ConnectionPatch

# ------------------------------------------Format-----------------------------------------

mpl.use('pdf')

# -----------------------------------------------------------------------------------------


def integer_formatter(x, pos):
    if x.is_integer():
        return str(int(x))
    else:
        return str(x)


integer_maker = FuncFormatter(integer_formatter)

# ------------------------------Class Definition for pi in axes----------------------------

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
    """
    Lightens the given color by multiplying (1-luminosity) by the given amount.
    Input can be matplotlib color string, hex string, or RGB tuple.

    Examples:
    >> lighten_color('g', 0.3)
    >> lighten_color('#F034A3', 0.6)
    >> lighten_color((.3,.55,.1), 0.5)
    """
    import matplotlib.colors as mc
    import colorsys
    try:
        c = mc.cnames[color]
    except:
        c = color
    c = colorsys.rgb_to_hls(*mc.to_rgb(c))
    return colorsys.hls_to_rgb(c[0], 1 - amount * (1 - c[1]), c[2])

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
fontsize_global = 32
fontsize_inner = 24

# -----------------------------------------------------------------------------------------

# ------------------------------Size and Shape of the Picture------------------------------

widescreen = 1.618
rectangular = 1

width = 10
height = width / widescreen
# height = 13

# -----------------------------------------------------------------------------------------

# -------------------------------------Create Subplots-------------------------------------

fig, (ax1, ax2, ax3, ax4) = plt.subplots(4)
fig.subplots_adjust(left=.15, bottom=.195, right=.89, top=.92)
grid = plt.GridSpec(2, 3, width_ratios=[4, 2, 1], height_ratios=[2, 2], wspace=0.2, hspace=0.2)
# ax1 = plt.subplot(121)
# ax2 = plt.subplot(122)
ax1 = plt.subplot(grid[0:, 0])
ax2 = plt.subplot(grid[0:, 1])
ax3 = plt.subplot(grid[0, 2])
ax4 = plt.subplot(grid[1, 2])

# -----------------------------------------------------------------------------------------

# ----------------------------------------------PLOT 1-------------------------------------

#                      -----------------------Set Grid----------------------

ax1.grid(False)
Filter = "On"  # It is either "On" or "Off".
#                      -----------------------------------------------------
# r'$\mathrm{\mu}}$'
# r'$E_{1}, \rho_{1}$'
#                      ------------------------X_Axis-----------------------

# ax1.set_xlabel(r'$\mathrm{\mu}}$', fontname="Latin Modern Math", fontsize=57,
#               fontweight='normal')
ax1.text(-0.4, -0.23 * 2 * np.pi, r'$ka$', fontname="Latin Modern Math", fontsize=fontsize_global,
         fontweight='normal')
for tick in ax1.get_xticklabels():
    tick.set_fontname("Latin Modern Math")
    tick.set_horizontalalignment('center')
# ax1.xaxis.set_label_coords(0.51, -0.15)
ax1.set_xlim(-1.0 * np.pi, 1.0 * np.pi)

#                      -----------------------------------------------------

#                      ------------------------Y_Axis-----------------------

# ax1.set_ylabel(r'$\Omega$', fontname="Latin Modern Math", fontsize=57,
#               fontweight='normal')
ax1.text(-1.55 * np.pi, 0.39 * 2 * np.pi, r'$\omega a / v_{0}$', fontname="Latin Modern Math", fontsize=fontsize_global,
         fontweight='normal', rotation=90)
for tick in ax1.get_yticklabels():
    tick.set_fontname("Latin Modern Math")
# ax1.yaxis.set_label_coords(-0.22, 0.5)

ya = 0
yb = 6.3

ax1.set_ylim(ya, yb)

ax1.arrow(-np.pi, 0.558093349 * 2 * np.pi, 0.3 * np.pi, 0.05 * 2 * np.pi, width=0.001, length_includes_head='False',
          head_width=0.1, color='Black', shape='full', alpha=0.5)
ax1.arrow(-np.pi, 0.477412235 * 2 * np.pi, 0.3 * np.pi, -0.05 * 2 * np.pi, width=0.001, length_includes_head='False',
          head_width=0.1, color='Black', shape='full', alpha=0.5)

ax1.text(-0.7 * np.pi + 0.05, (0.558093349 + 0.05 - 0.035) * 2 * np.pi, r'$\omega_{1}$', fontname="Latin Modern Math", fontsize=fontsize_inner,
         fontweight='normal', rotation=0)
ax1.text(-0.7 * np.pi + 0.05, (0.477412235 - 0.05 - 0.035) * 2 * np.pi, r'$\omega_{2}$', fontname="Latin Modern Math", fontsize=fontsize_inner,
         fontweight='normal', rotation=0)

#                      -----------------------------------------------------

#                      ------------------Relative Settings------------------

ax1.tick_params(axis='both', direction='out', length=7, width=1, color='black', pad=10,
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
ax1.xaxis.set_major_formatter(plt.FuncFormatter(multiple_formatter()))
#                              --------------------------------------

ax1.xaxis.set_major_locator(tck.MultipleLocator(np.pi / 2))

# ax1.yaxis.set_major_formatter(integer_maker)
ax1.yaxis.set_major_locator(tck.MultipleLocator(2))
# labels = [item.get_text() for item in ax1.get_xticklabels()]
# labels[1] = '0'
# labels[2] = '0.2'
# labels[3] = '0.4'
# labels[4] = '0.6'
# labels[5] = '0.8'
# labels[6] = '1'
# ax1.set_yticklabels(labels)


#                      -----------------------------------------------------

# -----------------------------------------------------------------------------------------

# ----------------------------------------------PLOT 2-------------------------------------

#                      -----------------------Set Grid----------------------

ax2.grid(False)
#                      -----------------------------------------------------

#                      ------------------------X_Axis-----------------------

# ax2.set_xlabel(r'$\mathcal{R}$', fontname="Latin Modern Math", fontsize=57,
#               fontweight='normal')
ax2.text(0.41, -0.23 * 2 * np.pi, r'$\mathcal{R}$', fontname="Latin Modern Math", fontsize=fontsize_global,
         fontweight='normal', rotation=0)
for tick in ax2.get_xticklabels():
    tick.set_fontname("Latin Modern Math")
# ax2.xaxis.set_label_coords(0.51, -0.17)
ax2.set_xlim(0, 1)

#                      -----------------------------------------------------

#                      ------------------------Y_Axis-----------------------

# ax2.set_ylabel(r'', fontname="Latin Modern Math", fontsize=57,
#               fontweight='normal')
ax2.text(-0.22, 0.5, r'', fontname="Latin Modern Math", fontsize=47,
         fontweight='normal', rotation=90)
for tick in ax2.get_yticklabels():
    tick.set_fontname("Latin Modern Math")
# ax2.yaxis.set_label_coords(-0.22, 0.5)
ax2.set_ylim(ya, yb)

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
ax2.set_yticklabels([])

#                      -----------------------------------------------------

# -----------------------------------------------------------------------------------------

# ----------------------------------------------PLOT 3-------------------------------------

#                      -----------------------Set Grid----------------------

ax3.grid(False)
#                      -----------------------------------------------------

x1_field = -1.5
x2_field = 1.5
y1_field = 0
y2_field = 3.0

#                      ------------------------X_Axis-----------------------

# ax3.set_xlabel(r'', fontname="Latin Modern Math", fontsize=fontsize_inner,
#               fontweight='normal')
ax3.text(0.51, -0.08, r'', fontname="Latin Modern Math", fontsize=32,
         fontweight='normal', rotation=0)
for tick in ax3.get_xticklabels():
    tick.set_fontname("Latin Modern Math")
# ax3.xaxis.set_label_coords(0.51, -0.08)
ax3.set_xlim(x1_field, x2_field)

#                      -----------------------------------------------------

#                      ------------------------Y_Axis-----------------------

# ax3.set_ylabel(r'$\frac{|\mathrm{u}_{(l)}(z)|^2}{|\mathrm{u}^{-}_{(0)}(z)|^2}$', fontname="Latin Modern Math", fontsize=fontsize_inner,
#               fontweight='normal')
ax3.text(2.3, 0.7, r'$|\mathrm{u}(z)|^2$', fontname="Latin Modern Math", fontsize=fontsize_global,
         fontweight='normal', rotation=90)
# for tick in ax3.get_yticklabels():
#    tick.set_fontname("Latin Modern Math")
# ax3.yaxis.set_label_coords(-0.05, -0.25)
ax3.set_ylim(y1_field, y2_field)

ax3.text(-0.3, 2.4, r'$\omega_{1}$', fontname="Latin Modern Math", fontsize=fontsize_inner,
         fontweight='normal', rotation=0)

#                      -----------------------------------------------------

#                      ------------------Relative Settings------------------

ax3.tick_params(axis='x', direction='out', length=7, width=1, color='black', pad=28,
                grid_color='black', grid_alpha=0.3, grid_linewidth=0.5, which='both', bottom=False, top=False)

ax3.tick_params(axis='y', direction='out', length=7, width=0, color='black', pad=28,
                grid_color='black', grid_alpha=0.3, grid_linewidth=0.5, which='both', bottom=False, top=False, labelbottom=False)

#                      -----------------------------------------------------

#                      -------------------Auto Set Axes---------------------

#                          ---------------------Notes-------------------
#
#                          If a pi exist in one of the axes then enable
#                          the Muplitple_formater().
#
#                          ---------------------------------------------

#                              ---------------Pi Nugget--------------
# ax3.xaxis.set_major_formatter(plt.FuncFormatter(multiple_formatter()))
#                              --------------------------------------

ax3.xaxis.set_major_locator(tck.MultipleLocator(1))
# ax3.xaxis.set_major_formatter(integer_maker)
ax3.set_xticklabels([])
ax3.set_yticklabels([])

#                      -----------------------------------------------------

# -----------------------------------------------------------------------------------------

# ----------------------------------------------PLOT 4-------------------------------------

#                      -----------------------Set Grid----------------------

ax4.grid(False)
#                      -----------------------------------------------------

#                      ------------------------X_Axis-----------------------

ax4.text(2.3, 0.7, r'$|\mathrm{u}(z)|^2$', fontname="Latin Modern Math", fontsize=fontsize_global,
         fontweight='normal', rotation=90)
ax1.text(2.55 * np.pi, -0.23 * 2 * np.pi, r'$z / a$', fontname="Latin Modern Math", fontsize=fontsize_global,
         fontweight='normal', rotation=0)
for tick in ax4.get_xticklabels():
    tick.set_fontname("Latin Modern Math")
# ax4.xaxis.set_label_coords(0.51, -0.08)
ax4.set_xlim(x1_field, x2_field)

#                      -----------------------------------------------------

#                      ------------------------Y_Axis-----------------------

# ax4.set_ylabel(r'', fontname="Latin Modern Math", fontsize=fontsize_inner,
#               fontweight='normal')
# ax4.text(-0.55, -2.6, r'$x / \mathrm{\alpha}$', fontname="Latin Modern Math", fontsize=fontsize_inner,
#         fontweight='normal', rotation=0)
for tick in ax4.get_yticklabels():
    tick.set_fontname("Latin Modern Math")
# ax4.yaxis.set_label_coords(-0.05, 0.5)
ax4.set_ylim(y1_field, y2_field)

ax4.text(-0.3, 2.4, r'$\omega_{2}$', fontname="Latin Modern Math", fontsize=fontsize_inner,
         fontweight='normal', rotation=0)

#                      -----------------------------------------------------

#                      ------------------Relative Settings------------------

ax4.tick_params(axis='x', direction='out', length=7, width=1, color='black', pad=28,
                grid_color='black', grid_alpha=0.3, grid_linewidth=0.5, bottom=False, top=False)

ax4.tick_params(axis='y', direction='out', length=7, width=0, color='black', pad=28,
                grid_color='black', grid_alpha=0.3, grid_linewidth=0.5, bottom=False, top=False)

#                      -----------------------------------------------------

#                      -------------------Auto Set Axes---------------------

#                          ---------------------Notes-------------------
#
#                          If a pi exist in one of the axes then enable
#                          the Muplitple_formater().
#
#                          ---------------------------------------------

#                              ---------------Pi Nugget--------------
# ax4.xaxis.set_major_formatter(plt.FuncFormatter(multiple_formatter()))
#                              --------------------------------------

ax4.xaxis.set_major_locator(tck.MultipleLocator(0.5))
# ax4.xaxis.set_major_formatter(integer_maker)
ax4.set_xticklabels([])
ax4.set_yticklabels([])

#                      -----------------------------------------------------

# -----------------------------------------------------------------------------------------

# --------------------------------Input Data for Plotting----------------------------------

location_of_input_file = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data.txt'

data = pd.read_csv(location_of_input_file, sep='\s+', header=None)

location_of_input_file_field = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/rtq_data_field.txt'

data_field = pd.read_csv(location_of_input_file_field, sep='\s+', header=None)

alpha_length = '/home/varoth/Desktop/Physics/PLOTS/Python/rtq_plots/alpha_length.txt'

alpha_len = pd.read_csv(alpha_length, sep='\s+', header=None)

N = len(data.columns)

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


Minimum_Value = 0.00000001
Maximum_Value = np.pi - Minimum_Value

data_filtered_A_1 = data[(abs(data.iloc[:, 1]) >= Minimum_Value)]
data_filtered_A_2 = data[abs(data.iloc[:, 2]) >= Minimum_Value]
data_filtered_B_1 = data_filtered_A_1[abs(data_filtered_A_1.iloc[:, 1]) <= Maximum_Value]
data_filtered_B_2 = data_filtered_A_2[abs(data_filtered_A_2.iloc[:, 2]) <= Maximum_Value]

x = data.iloc[:, 0]
x1 = data_filtered_B_1.iloc[:, 0]
x2 = data_filtered_B_2.iloc[:, 0]

# x3 = data_field.iloc[:, 0]
# x4 = data_field.iloc[:, 1]
# x5 = data_field.iloc[:, 2]

y1 = data_filtered_B_1.iloc[:, 1]
y2 = data_filtered_B_2.iloc[:, 2]
y3 = data.iloc[:, 3]
y4 = data.iloc[:, 4]
y5 = data.iloc[:, 5]


y1_f_down = data_field.iloc[:, 3]
y2_f_down = data_field.iloc[:, 4]
y3_f_down = data_field.iloc[:, 5]
y1_f_up = data_field.iloc[:, 6]
y2_f_up = data_field.iloc[:, 7]
y3_f_up = data_field.iloc[:, 8]

if Filter == 'On':
    print('Zero, +Pi or -Pi values of k*a are neglected.')
    ax1.scatter(y1, x1, c=color1, alpha=1.0, s=scatter_size)
    ax1.scatter(y2, x2, c=color1, alpha=1.0, s=scatter_size)
elif Filter == 'Off':
    for i in range(1, N - 3, 1):
        y = data.iloc[:, i]
        ax1.scatter(y, x, c=color1, alpha=1.0, s=scatter_size)

ax2.plot(y3, x, c=color2, alpha=1.0, linewidth=line_size)
ax2.plot(y4, x, c=color1, alpha=1.0, linewidth=line_size)
ax2.plot(y5, x, c=color1, alpha=1.0, linestyle='--', linewidth=line_size)

Field_Filter = 'On'

if Field_Filter == 'Off':
    print('Only one period')
    x1_f = data_field.iloc[:, 0]
    x2_f = data_field.iloc[:, 1]
    x3_f = data_field.iloc[:, 2]

    ax3.plot(x1_f, y1_f_up, c=color1, alpha=1.0, linewidth=line_size)
    ax3.plot(x2_f, y2_f_up, c=color1, alpha=1.0, linewidth=line_size)
    ax3.plot(x3_f, y3_f_up, c=color1, alpha=1.0, linewidth=line_size)

    ax4.plot(x1_f, y1_f_down, c=color1, alpha=1.0, linewidth=line_size)
    ax4.plot(x2_f, y2_f_down, c=color1, alpha=1.0, linewidth=line_size)
    ax4.plot(x3_f, y3_f_down, c=color1, alpha=1.0, linewidth=line_size)

elif Field_Filter == 'On':
    for i in range(0, 10, 1):

        x1_f = data_field.iloc[:, 0] + i - 5
        x2_f = data_field.iloc[:, 1] + i - 5
        x3_f = data_field.iloc[:, 2] + i - 5

        ax3.plot(x1_f, y1_f_up, c=color1, alpha=1.0, linewidth=line_size)
        ax3.plot(x2_f, y2_f_up, c=color1, alpha=1.0, linewidth=line_size)
        ax3.plot(x3_f, y3_f_up, c=color1, alpha=1.0, linewidth=line_size)

        ax4.plot(x1_f, y1_f_down, c=color1, alpha=1.0, linewidth=line_size)
        ax4.plot(x2_f, y2_f_down, c=color1, alpha=1.0, linewidth=line_size)
        ax4.plot(x3_f, y3_f_down, c=color1, alpha=1.0, linewidth=line_size)


# ax4.plot(x3, y6, c=color1, alpha=1.0, linewidth=line_size)
# ax4.plot(x4, y7, c=color1, alpha=1.0, linewidth=line_size)
# ax4.plot(x5, y8, c=color1, alpha=1.0, linewidth=line_size)
# ax3.plot(x3, y9, c=color1, alpha=1.0, linewidth=line_size)
# ax3.plot(x4, y10, c=color1, alpha=1.0, linewidth=line_size)
# ax3.plot(x5, y11, c=color1, alpha=1.0, linewidth=line_size)

#                      -----------------------------------------------------

# ax3.invert_xaxis()
# ax3.invert_yaxis()

# ------------------------------Placing Band Gaps if Any-----------------------------------

mpl.rcParams['font.family'] = "Latin Modern"
mpl.rcParams['font.serif'] = "Latin Modern Math"

#                      ---------------------Second Run----------------------

rectangle = plt.Rectangle((-1.5 * np.pi, 0.473412235 * 2 * np.pi),
                          3 * np.pi, 0.086681114 * 2 * np.pi,
                          fc='grey',
                          alpha=0.15, label='Ενεργειακό\nΧάσμα')
ax2.add_patch(rectangle)

rectangle = plt.Rectangle((-1.5 * np.pi, 0.473412235 * 2 * np.pi),
                          3 * np.pi, 0.086681114 * 2 * np.pi,
                          fc='grey',
                          alpha=0.15, label='Ενεργειακό\nΧάσμα')
ax1.add_patch(rectangle)

ALPHA = alpha_len.iloc[0, 0]

BETA = 1.0 - ALPHA

for i in range(-5, 5, 1):
    rectangle = plt.Rectangle(((i + (BETA / 2)), 0),
                              ALPHA, 10,
                              fc='grey',
                              alpha=0.2, label='Ενεργειακό\nΧάσμα')
    ax3.add_patch(rectangle)

    rectangle = plt.Rectangle(((i + ALPHA + (BETA / 2)), 0),
                              (BETA), 10,
                              fc='black',
                              alpha=0.2, label='Ενεργειακό\nΧάσμα')
    ax3.add_patch(rectangle)

    # dotted_line = plt.Line2D(((1.0 - i), (1.0 - i)), (0, 4), lw=2.,
    #                         ls='-.', marker='.',
    #                         markersize=30,
    #                         markerfacecolor='black',
    #                         markeredgecolor='black',
    #                         alpha=0.4)
    # ax3.add_line(dotted_line)

for i in range(-5, 5, 1):
    rectangle = plt.Rectangle(((i + (BETA / 2)), 0),
                              ALPHA, 10,
                              fc='grey',
                              alpha=0.2, label='Ενεργειακό\nΧάσμα')
    ax4.add_patch(rectangle)

    rectangle = plt.Rectangle(((i + ALPHA + (BETA / 2)), 0),
                              (BETA), 10,
                              fc='black',
                              alpha=0.2, label='Ενεργειακό\nΧάσμα')
    ax4.add_patch(rectangle)

    # dotted_line = plt.Line2D(((1.5 - i), (1.5 - i)), (0, 4), lw=2.,
    #                         ls='-.', marker='.',
    #                         markersize=30,
    #                         markerfacecolor='black',
    #                         markeredgecolor='black',
    #                         alpha=0.4)
    # ax4.add_line(dotted_line)

# ax3.fill_between(x3, 0, y7, facecolor='darkred', alpha=1.0)
# ax3.fill_between(x4, 0, y8, facecolor='darkred', alpha=1.0)
# ax4.fill_between(x5, 0, y9, facecolor='darkred', alpha=1.0)
# ax4.fill_between(x6, 0, y10, facecolor='darkred', alpha=1.0)

# rectangle = plt.Rectangle((0.0, 0),
#                          0.6, 10,
#                          fc='green',
#                          alpha=0.7, label='Ενεργειακό\nΧάσμα')
# ax3.add_patch(rectangle)

# rectangle = plt.Rectangle((0.6, 0),
#                          0.4, 10,
#                          fc='royalblue',
#                          alpha=0.7, label='Ενεργειακό\nΧάσμα')
# ax3.add_patch(rectangle)
#                      -----------------------------------------------------

#                      ---------------------Third Run-----------------------

# rectangle = plt.Rectangle((-1.5 * np.pi, 0.411),
#                          1.5 * np.pi, 0.05,
#                          fc='gainsboro',
#                          alpha=0.7, label='Ενεργειακό\nΧάσμα Κατεύθυνσης')
# plt.gca().add_patch(rectangle)

# rectangle = plt.Rectangle((-1.5 * np.pi, 0.461),
#                          3 * np.pi, 0.0487,
#                          fc='slategrey',
#                          alpha=0.7, label='Ενεργειακό\nΧάσμα')
# plt.gca().add_patch(rectangle)

# rectangle = plt.Rectangle((0, 0.51),
#                          1.5 * np.pi, 0.049,
#                          fc='gainsboro',
#                          alpha=0.7)
# plt.gca().add_patch(rectangle)

#                      -----------------------------------------------------

#                      ---------------------Fourth Run----------------------

# rectangle = plt.Rectangle((-1.5 * np.pi, 0.338),
#                          1.5 * np.pi, 0.093,
#                          fc='gainsboro',
#                          alpha=0.7)
# plt.gca().add_patch(rectangle)

# rectangle = plt.Rectangle((0, 0.5379),
#                          1.5 * np.pi, 0.093,
#                          fc='gainsboro',
#                          alpha=0.7)
# plt.gca().add_patch(rectangle)

#                      -----------------------------------------------------

# kw = dict(linestyle="--", color="red")
# cp1 = ConnectionPatch((.5, 0), (.5, 1), "axes fraction", "axes fraction",
#                      axesA=ax1[0, 0], axesB=ax2[1, 0])
# cp2 = ConnectionPatch((.5, 0), (.5, 1), "axes fraction", "axes fraction",
#                      axesA=ax1[0, 1], axesB=ax2[1, 1])

# for cp in (cp1, cp2):
#    ax1[1, 1].add_artist(cp)


# coord1 = transFigure.transform(ax1.transData.transform([0, 1]))
# coord2 = transFigure.transform(ax2.transData.transform([0.5, 0.5]))


# line = mpl.lines.Line2D((coord1[0], coord2[0]), (coord1[1], coord2[1]),
#                        transform=fig.transFigure)
# fig.lines = line

#                      -------------------Vertical Line---------------------

# dotted_line = plt.Line2D((0.5, 0.5), (0, 1), lw=2.,
#                         ls='-.', marker='.',
#                         markersize=30,
#                         markerfacecolor='black',
#                         markeredgecolor='black',
#                         alpha=0.4)
# plt.add_line(dotted_line)

#                      -----------------------------------------------------
line_size_connection = 1.5
y_connection_1 = 0.46315 * 2 * np.pi

xy_A_1 = (0.0, y_connection_1)
xy_B_1 = (0.937 * np.pi, y_connection_1)

y_connection_2 = y_connection_1 - 0.0229 * 2 * np.pi

xy_A_2 = (0.0, y_connection_2)
xy_B_2 = (0.876 * np.pi, y_connection_2)

y_connection_3 = y_connection_2 - 0.0285 * 2 * np.pi

xy_A_3 = (0.0, y_connection_3)
xy_B_3 = (0.815 * np.pi, y_connection_3)

y_connection_4 = y_connection_3 - 0.03015 * 2 * np.pi

xy_A_4 = (0.0, y_connection_4)
xy_B_4 = (0.751 * np.pi, y_connection_4)

y_connection_5 = y_connection_4 - 0.031 * 2 * np.pi

xy_A_5 = (0.0, y_connection_5)
xy_B_5 = (0.687 * np.pi, y_connection_5)

y_connection_6 = y_connection_5 - 0.031 * 2 * np.pi

xy_A_6 = (0.0, y_connection_6)
xy_B_6 = (0.625 * np.pi, y_connection_6)

y_connection_7 = y_connection_6 - 0.032 * 2 * np.pi

xy_A_7 = (0.0, y_connection_7)
xy_B_7 = (0.563 * np.pi, y_connection_7)

y_connection_8 = y_connection_7 - 0.031 * 2 * np.pi

xy_A_8 = (0.0, y_connection_8)
xy_B_8 = (0.503 * np.pi, y_connection_8)

y_connection_9 = y_connection_8 - 0.032 * 2 * np.pi

xy_A_9 = (0.0, y_connection_9)
xy_B_9 = (0.441 * np.pi, y_connection_9)

y_connection_10 = y_connection_9 - 0.032 * 2 * np.pi

xy_A_10 = (0.0, y_connection_10)
xy_B_10 = (0.379 * np.pi, y_connection_10)

y_connection_11 = y_connection_10 - 0.032 * 2 * np.pi

xy_A_11 = (0.0, y_connection_11)
xy_B_11 = (0.315 * np.pi, y_connection_11)

y_connection_12 = y_connection_11 - 0.032 * 2 * np.pi

xy_A_12 = (0.0, y_connection_12)
xy_B_12 = (0.253 * np.pi, y_connection_12)

y_connection_13 = y_connection_12 - 0.031 * 2 * np.pi

xy_A_13 = (0.0, y_connection_13)
xy_B_13 = (0.189 * np.pi, y_connection_13)

y_connection_14 = y_connection_13 - 0.0305 * 2 * np.pi

xy_A_14 = (0.0, y_connection_14)
xy_B_14 = (0.127 * np.pi, y_connection_14)

y_connection_15 = y_connection_14 - 0.034 * 2 * np.pi

xy_A_15 = (0.0, y_connection_15)
xy_B_15 = (0.063 * np.pi, y_connection_15)

y_connection_16 = y_connection_15 - 0.031 * 2 * np.pi

xy_A_16 = (0.0, y_connection_16)
xy_B_16 = (0.0 * np.pi, y_connection_16)


con = ConnectionPatch(xyA=xy_A_1, xyB=xy_B_1, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_2, xyB=xy_B_2, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_3, xyB=xy_B_3, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_4, xyB=xy_B_4, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_5, xyB=xy_B_5, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_6, xyB=xy_B_6, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_7, xyB=xy_B_7, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_8, xyB=xy_B_8, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_9, xyB=xy_B_9, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_10, xyB=xy_B_10, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_11, xyB=xy_B_11, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_12, xyB=xy_B_12, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_13, xyB=xy_B_13, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_14, xyB=xy_B_14, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

con = ConnectionPatch(xyA=xy_A_15, xyB=xy_B_15, coordsA="data", coordsB="data", alpha=0.5,
                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
ax2.add_artist(con)

# con = ConnectionPatch(xyA=xy_A_16, xyB=xy_B_16, coordsA="data", coordsB="data", alpha=0.5,
#                      axesA=ax2, axesB=ax1, color="Black", linestyle='--', linewidth=line_size_connection)
# ax2.add_artist(con)

dotted_line = plt.Line2D((0.937 * np.pi, 0.937 * np.pi), (y_connection_1, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.876 * np.pi, 0.876 * np.pi), (y_connection_2, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.815 * np.pi, 0.815 * np.pi), (y_connection_3, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.751 * np.pi, 0.751 * np.pi), (y_connection_4, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.687 * np.pi, 0.687 * np.pi), (y_connection_5, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.625 * np.pi, 0.625 * np.pi), (y_connection_6, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.563 * np.pi, 0.563 * np.pi), (y_connection_7, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.503 * np.pi, 0.503 * np.pi), (y_connection_8, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.441 * np.pi, 0.441 * np.pi), (y_connection_9, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.379 * np.pi, 0.379 * np.pi), (y_connection_10, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.315 * np.pi, 0.315 * np.pi), (y_connection_11, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.253 * np.pi, 0.253 * np.pi), (y_connection_12, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.189 * np.pi, 0.189 * np.pi), (y_connection_13, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.127 * np.pi, 0.127 * np.pi), (y_connection_14, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

dotted_line = plt.Line2D((0.063 * np.pi, 0.063 * np.pi), (y_connection_15, 0), lw=line_size_connection,
                         ls='--', marker='.',
                         markersize=0,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color="Black",
                         alpha=0.5)
ax1.add_line(dotted_line)

# ------------------------Printing Plots in a format (pdf, eps, etc.)----------------------
fig.set_size_inches(width, height)
fig.savefig('plot.pdf')
# -----------------------------------------------------------------------------------------

# _________________________________________________________________________________________
# -----------------------------------------------------------------------------------------
