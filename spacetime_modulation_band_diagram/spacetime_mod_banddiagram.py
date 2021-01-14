# _________________________________________________________________________________________
# ------------------------Band Diagram of a Space - Time Modulated medium------------------

# --------------------------------------Import Packages------------------------------------

import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.ticker as tck
import pandas as pd

# -----------------------------------------------------------------------------------------

# ------------------------------------------Format-----------------------------------------

mpl.use('pdf')

# -----------------------------------------------------------------------------------------

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
plt.rc('xtick', labelsize=52)
plt.rc('ytick', labelsize=52)
plt.rc('axes', labelsize=12)
plt.rc('axes', linewidth=4)
fontsize_Bigview_Global = 40
fontsize_Smallview_Global = 63

# -----------------------------------------------------------------------------------------

# ------------------------------Size and Shape of the Picture------------------------------

widescreen = 1.618
rectangular = 1

width = 10
height = width / rectangular
# height = 13

# -----------------------------------------------------------------------------------------

# ------------------------------------Positioning of Plot----------------------------------

fig, ax = plt.subplots()
fig.subplots_adjust(left=.2, bottom=.18, right=.89, top=.89)

# -----------------------------------------------------------------------------------------

# --------------------------------------Options for Labels---------------------------------

#                      ----------------------Set Title----------------------

# ax.set_title("Band Diagram", fontname="Times New Roman", fontsize=57, fontweight='normal',
#             loc='center', pad=20)

#                      -----------------------------------------------------

#                      -----------------------Set Grid----------------------

ax.grid(False)

#                      -----------------------------------------------------

# ====================================== Control Panel =====================================
Analytical = 'Both'                    # It's either 'On', 'Off' or "Both"
Approximation = 'Off'                  # It's either 'On' or 'Off'
unmodulated_analytical = 'On'        # It's either 'On' or 'Off'
filter = 'On'                         # It's either 'On' or 'Off'
Vertical_Lines = 'Off'                # It's either 'On' or 'Off'
Modulated_Lines = 'On'               # It's either 'On' or 'Off'
Relative_y_axis = 'Off'               # It's either 'On' or 'Off'
Big_View = 'On'                      # It's either 'On' or 'Off'
Central_Vertical_Line = 'Off'         # It's either 'On' or 'Off'
# ------------------------------------------------------------------------------------------
# ------------------------------- Setting for Modulated_Lines ------------------------------
NUMBER_OF_LINE_FIELD = 1
space_multiplier_between_the_lines = 8.0
middle_of_the_lines = 0.0       # Real Middle is 3.0
# ------------------------------------------------------------------------------------------

# ---------------------------------- Plot - Boundaries -------------------------------------
if (Big_View == 'On'):
    x_min = -4 * np.pi
    x_max = 4 * np.pi
    y_min = -1.5
    y_max = 6.3
elif (Big_View == 'Off'):
    x_min = -1.5 * np.pi
    x_max = 1.5 * np.pi
    y_min = 0
    y_max = 6.3
# ==========================================================================================

if (Big_View == 'On'):

    #                      ------------------------X_Axis-----------------------

    ax.text(-5.7 * np.pi, 1.8, r'$\omega \Lambda / v_{0}$', fontname="Latin Modern Math", fontsize=fontsize_Bigview_Global,
            fontweight='normal', rotation=90)
    for tick in ax.get_xticklabels():
        tick.set_fontname("Latin Modern Math")
    ax.set_xlim(x_min, x_max)

    #                      -----------------------------------------------------

    #                      ------------------------Y_Axis-----------------------

    ax.text(-0.4 * np.pi, -3.1, r'$k \Lambda$', fontname="Latin Modern Math", fontsize=fontsize_Bigview_Global,
            fontweight='normal', rotation=0)
    for tick in ax.get_yticklabels():
        tick.set_fontname("Latin Modern Math")
    ax.set_ylim(y_min, y_max)

    #                      -----------------------------------------------------

elif (Big_View == 'Off'):

    #                      ------------------------X_Axis-----------------------

    ax.text(-2.30 * np.pi, 2.5, r'$\omega \Lambda / v_{0}$', fontname="Latin Modern Math", fontsize=fontsize_Smallview_Global,
            fontweight='normal', rotation=90)
    for tick in ax.get_xticklabels():
        tick.set_fontname("Latin Modern Math")
    ax.set_xlim(x_min, x_max)

    #                      -----------------------------------------------------

    #                      ------------------------Y_Axis-----------------------

    ax.text(-0.20 * np.pi, -1.6, r'$k \Lambda$', fontname="Latin Modern Math", fontsize=fontsize_Smallview_Global,
            fontweight='normal', rotation=0)
    for tick in ax.get_yticklabels():
        tick.set_fontname("Latin Modern Math")
    ax.set_ylim(y_min, y_max)

    #                      -----------------------------------------------------

#                      ------------------Relative Settings------------------

ax.tick_params(axis='both', direction='out', length=7, width=1, color='black', pad=10,
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
ax.xaxis.set_major_formatter(plt.FuncFormatter(multiple_formatter()))
#                              --------------------------------------

modulation_speed_input = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/speed.txt'
nm = pd.read_csv(modulation_speed_input, sep='\s+', header=None)
NM = nm.iat[0, 0]
K_M_input = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/k_m.txt'
KM = pd.read_csv(K_M_input, sep='\s+', header=None)
K_M = KM.iat[0, 0]

if (Big_View == 'Off'):
    ax.xaxis.set_major_locator(tck.MultipleLocator(np.pi))
    if (Relative_y_axis == 'On'):
        if (NM == 0.0):
            ax.yaxis.set_major_locator(tck.MultipleLocator(1.0))
        else:
            ax.yaxis.set_major_locator(tck.MultipleLocator(NM * 2 * np.pi))
    elif(Relative_y_axis == 'Off'):
        ax.yaxis.set_major_locator(tck.MultipleLocator(2.0))

elif (Big_View == 'On'):
    ax.xaxis.set_major_locator(tck.MultipleLocator(K_M))
    if (Relative_y_axis == 'On'):
        if (NM == 0.0):
            ax.yaxis.set_major_locator(tck.MultipleLocator(1.0))
        else:
            ax.yaxis.set_major_locator(tck.MultipleLocator(NM * 2 * np.pi))
    elif(Relative_y_axis == 'Off'):
        ax.yaxis.set_major_locator(tck.MultipleLocator(2.0))

#                      -----------------------------------------------------

#                      ------------------Manual Set Axes--------------------

# ax.set_xticks([-1.5 * np.pi, -np.pi, -0.5 * np.pi, 0.0, 0.5 * np.pi, np.pi, 1.5 * np.pi])
# ax.set_yticks([-1, -0.8, -0.6, -0.4, -0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1])

#                      -----------------------------------------------------

# -----------------------------------------------------------------------------------------

# --------------------------------Input Data for Plotting----------------------------------

location_of_input_file = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/data.txt'

data = pd.read_csv(location_of_input_file, sep='\s+', header=None)

location_of_input_file_analytical_1 = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/analytical_data_1.txt'

analytical_data_1 = pd.read_csv(location_of_input_file_analytical_1, sep='\s+', header=None)

location_of_input_file_analytical_2 = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/analytical_data_2.txt'

analytical_data_2 = pd.read_csv(location_of_input_file_analytical_2, sep='\s+', header=None)

minus_location_of_input_file_analytical_1 = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/minus_analytical_data_1.txt'

minus_analytical_data_1 = pd.read_csv(minus_location_of_input_file_analytical_1, sep='\s+', header=None)

minus_location_of_input_file_analytical_2 = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/minus_analytical_data_2.txt'

minus_analytical_data_2 = pd.read_csv(minus_location_of_input_file_analytical_2, sep='\s+', header=None)

unmod_location_of_input_file_analytical_1 = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/unmod_analytical_data_1.txt'

unmod_analytical_data_1 = pd.read_csv(unmod_location_of_input_file_analytical_1, sep='\s+', header=None)

unmod_location_of_input_file_analytical_2 = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/unmod_analytical_data_2.txt'

unmod_analytical_data_2 = pd.read_csv(unmod_location_of_input_file_analytical_2, sep='\s+', header=None)

unmodulated_speed_input = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/permaspeed.txt'

C_0 = pd.read_csv(unmodulated_speed_input, sep='\s+', header=None)

data_location_two_homogenous_slices = '/home/varoth/Desktop/Physics/PLOTS/Python/ptc_plots/rtq_data_two_homogenous_slices.txt'

data_two_homogenous_slices = pd.read_csv(data_location_two_homogenous_slices, sep='\s+', header=None)

# Tilt = np.sqrt((1 + ((0.2)**(2))) / (1 - ((0.2)**(2)))) - 1

N_M = C_0 * NM

OMEGA_M = N_M * K_M

TILT = 2 * N_M

# Tilt = (1 / (np.sqrt(1 - (NM**(2))))) - 1

N = len(data.columns)

N_two_homogenous_slices = len(data_two_homogenous_slices.columns)

kmax = data.count

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
color5 = 'orange'
color6 = 'darkgoldenrod'
color7 = 'cyan'
color8 = 'magenta'
color9 = 'lavender'
color10 = 'chocolate'

plot_line_size = 4
scatter_size = plot_line_size
line_size = plot_line_size - 1
transparency_complete = 1.0
transparency_analytic = 0.6
transparency_unmod = 0.2

z_order_value = 2

#                      ---------------------Notes--------------------
#
#                      Transparency is an option only for pdf outputs
#                      and not for eps outputs.
#
#                      ----------------------------------------------

# -----------------------------------------------------------------------------------------

# -----------------------------------Plotting the Plot-------------------------------------

#                      ---------------------Scatter Plot--------------------

if (Analytical == 'Off'):
    Minimum_Value = 0.0099       # 0.0045418
    Maximum_Value = np.pi - Minimum_Value

    data_filtered_A_1 = data[(abs(data.iloc[:, :]) >= Minimum_Value)]

    precise_values_1 = -1 * 2.0 * np.pi - np.pi + 2.0 * np.pi * np.pi * (NM / K_M)
    precise_values_2 = 0 * 2.0 * np.pi - np.pi + 2.0 * np.pi * np.pi * (NM / K_M)
    precise_values_3 = 1 * 2.0 * np.pi - np.pi + 2.0 * np.pi * np.pi * (NM / K_M)
    precise_values_4 = 2 * 2.0 * np.pi - np.pi + 2.0 * np.pi * np.pi * (NM / K_M)
    data_filtered_B_1 = data[abs(data_filtered_A_1.iloc[:, :] - precise_values_1) >= Minimum_Value]
    data_filtered_B_2 = data[abs(data_filtered_B_1.iloc[:, :] - precise_values_2) >= Minimum_Value]
    data_filtered_B_3 = data[abs(data_filtered_B_2.iloc[:, :] - precise_values_3) >= Minimum_Value]
    data_filtered_B_4 = data[abs(data_filtered_B_3.iloc[:, :] - precise_values_4) >= Minimum_Value]

    precise_values_5 = 0 * 2.0 * np.pi - 2.0 * np.pi + 4.0 * np.pi * np.pi * (NM / K_M) + 0.01
    precise_values_6 = -1 * 2.0 * np.pi - 2.0 * np.pi + 4.0 * np.pi * np.pi * (NM / K_M) - 0.03
    precise_values_7 = 1 * 2.0 * np.pi - 2.0 * np.pi + 4.0 * np.pi * np.pi * (NM / K_M) + 0.02
    data_filtered_B_5 = data[abs(data_filtered_B_4.iloc[:, :] - precise_values_5) >= Minimum_Value]
    data_filtered_B_6 = data[abs(data_filtered_B_5.iloc[:, :] - precise_values_6) >= Minimum_Value]
    data_filtered_B_7 = data[abs(data_filtered_B_6.iloc[:, :] - precise_values_7) >= Minimum_Value]

    if (filter == 'On'):
        x = data_filtered_B_7.iloc[:, 0]
        for i in range(1, N, 1):
            y = data_filtered_B_7.iloc[:, i]
            color = color3    # rainbow[i]
            plt.scatter(y, x, c=color1, s=scatter_size, alpha=transparency_complete, cmap='viridis', zorder=z_order_value)

    if (filter == 'Off'):
        x = data.iloc[:, 0]
        for i in range(1, N, 1):
            y = data.iloc[:, i]
            color = color3    # rainbow[i]
            plt.scatter(y, x, c=color1, s=scatter_size, alpha=transparency_complete, cmap='viridis', zorder=z_order_value)

if (Analytical == 'On'):
    x_analytical_A = analytical_data_1.iloc[:, 0]
    x_analytical_B = analytical_data_2.iloc[:, 0]

    for j in range(1, 8, 1):
        y_analytical_A = analytical_data_1.iloc[:, j]
        y_analytical_B = analytical_data_2.iloc[:, j]
        color = color3    # rainbow[j]
        plt.plot(y_analytical_A, x_analytical_A, c=color, alpha=transparency_analytic, linewidth=line_size, zorder=z_order_value)
        plt.plot(y_analytical_B, x_analytical_B, c=color, alpha=transparency_analytic, linewidth=line_size, zorder=z_order_value)

        x_minus_analytical_A = minus_analytical_data_1.iloc[:, 0]
        x_minus_analytical_B = minus_analytical_data_2.iloc[:, 0]

    for j in range(1, 8, 1):
        y_minus_analytical_A = minus_analytical_data_1.iloc[:, j]
        y_minus_analytical_B = minus_analytical_data_2.iloc[:, j]
        color = color3    # rainbow[j]
        plt.plot(y_minus_analytical_A, x_minus_analytical_A, c=color, alpha=transparency_analytic, linestyle='--', linewidth=line_size, zorder=z_order_value)
        plt.plot(y_minus_analytical_B, x_minus_analytical_B, c=color, alpha=transparency_analytic, linestyle='--', linewidth=line_size, zorder=z_order_value)

if (Analytical == 'Both'):
    Minimum_Value = 0.0099       # 0.0045418
    Maximum_Value = np.pi - Minimum_Value

    data_filtered_A_1 = data[(abs(data.iloc[:, :]) >= Minimum_Value)]

    precise_values_1 = -1 * 2.0 * np.pi - np.pi + 2.0 * np.pi * np.pi * (NM / K_M)
    precise_values_2 = 0 * 2.0 * np.pi - np.pi + 2.0 * np.pi * np.pi * (NM / K_M)
    precise_values_3 = 1 * 2.0 * np.pi - np.pi + 2.0 * np.pi * np.pi * (NM / K_M)
    precise_values_4 = 2 * 2.0 * np.pi - np.pi + 2.0 * np.pi * np.pi * (NM / K_M)
    data_filtered_B_1 = data[abs(data_filtered_A_1.iloc[:, :] - precise_values_1) >= Minimum_Value]
    data_filtered_B_2 = data[abs(data_filtered_B_1.iloc[:, :] - precise_values_2) >= Minimum_Value]
    data_filtered_B_3 = data[abs(data_filtered_B_2.iloc[:, :] - precise_values_3) >= Minimum_Value]
    data_filtered_B_4 = data[abs(data_filtered_B_3.iloc[:, :] - precise_values_4) >= Minimum_Value]

    precise_values_5 = 0 * 2.0 * np.pi - 2.0 * np.pi + 4.0 * np.pi * np.pi * (NM / K_M) + 0.01
    precise_values_6 = -1 * 2.0 * np.pi - 2.0 * np.pi + 4.0 * np.pi * np.pi * (NM / K_M) - 0.03
    precise_values_7 = 1 * 2.0 * np.pi - 2.0 * np.pi + 4.0 * np.pi * np.pi * (NM / K_M) + 0.02
    data_filtered_B_5 = data[abs(data_filtered_B_4.iloc[:, :] - precise_values_5) >= Minimum_Value]
    data_filtered_B_6 = data[abs(data_filtered_B_5.iloc[:, :] - precise_values_6) >= Minimum_Value]
    data_filtered_B_7 = data[abs(data_filtered_B_6.iloc[:, :] - precise_values_7) >= Minimum_Value]

    if (filter == 'On'):
        x = data_filtered_B_7.iloc[:, 0]
        for i in range(1, N, 1):
            y = data_filtered_B_7.iloc[:, i]
            color = color3    # rainbow[i]
            plt.scatter(y, x, c=color1, s=scatter_size, alpha=transparency_complete, cmap='viridis', zorder=z_order_value)

    if (filter == 'Off'):
        x = data.iloc[:, 0]
        for i in range(1, N, 1):
            y = data.iloc[:, i]
            color = color3    # rainbow[i]
            plt.scatter(y, x, c=color1, s=scatter_size, alpha=transparency_complete, cmap='viridis', zorder=z_order_value)

    x_analytical_A = analytical_data_1.iloc[:, 0]
    x_analytical_B = analytical_data_2.iloc[:, 0]

    for j in range(1, 8, 1):
        y_analytical_A = analytical_data_1.iloc[:, j]
        y_analytical_B = analytical_data_2.iloc[:, j]
        color = color3    # rainbow[j]
        plt.plot(y_analytical_A, x_analytical_A, c=color, alpha=transparency_analytic, linewidth=line_size, zorder=z_order_value)
        plt.plot(y_analytical_B, x_analytical_A, c=color, alpha=transparency_analytic, linewidth=line_size, zorder=z_order_value)

        x_minus_analytical_A = minus_analytical_data_1.iloc[:, 0]
        x_minus_analytical_B = minus_analytical_data_2.iloc[:, 0]

    for j in range(1, 8, 1):
        y_minus_analytical_A = minus_analytical_data_1.iloc[:, j]
        y_minus_analytical_B = minus_analytical_data_2.iloc[:, j]
        color = color3    # rainbow[j]
        plt.plot(y_minus_analytical_A, x_minus_analytical_A, c=color, alpha=transparency_analytic, linestyle='--', linewidth=line_size, zorder=z_order_value)
        plt.plot(y_minus_analytical_B, x_minus_analytical_B, c=color, alpha=transparency_analytic, linestyle='--', linewidth=line_size, zorder=z_order_value)

if (unmodulated_analytical == 'On'):

    x_unmod_analytical_A = unmod_analytical_data_1.iloc[:, 0]
    x_unmod_analytical_B = unmod_analytical_data_2.iloc[:, 0]

    for j in range(1, 8, 1):
        y_unmod_analytical_A = unmod_analytical_data_1.iloc[:, j]
        y_unmod_analytical_B = unmod_analytical_data_2.iloc[:, j]
        plt.plot(y_unmod_analytical_A, x_unmod_analytical_A, c=color1, alpha=transparency_unmod, linestyle=':', linewidth=line_size, zorder=z_order_value)
        plt.plot(y_unmod_analytical_B, x_unmod_analytical_B, c=color1, alpha=transparency_unmod, linestyle=':', linewidth=line_size, zorder=z_order_value)
elif (unmodulated_analytical == 'Off'):
    exit

if (Approximation == 'On'):

    Minimum_Value_two = 0.00000001
    Maximum_Value_two = np.pi - Minimum_Value_two

    data_filtered_two_A_1 = data_two_homogenous_slices[(abs(data_two_homogenous_slices.iloc[:, 1]) >= Minimum_Value_two)]
    data_filtered_two_B_1 = data_filtered_two_A_1[abs(data_filtered_two_A_1.iloc[:, 1]) <= Maximum_Value_two]

    x_two_homogenous_slices = data_filtered_two_B_1.iloc[:, 0]
    for i in range(1, N_two_homogenous_slices, 1):
        y_two_homogenous_slices = data_filtered_two_B_1.iloc[:, i]
        color = color3    # rainbow[i]
        plt.scatter(y_two_homogenous_slices, x_two_homogenous_slices, c=color2, s=scatter_size, alpha=transparency_analytic, cmap='viridis', zorder=1)
        plt.scatter(y_two_homogenous_slices + 2 * np.pi, x_two_homogenous_slices, c=color2, s=scatter_size, alpha=transparency_analytic, cmap='viridis', zorder=1)
        plt.scatter(y_two_homogenous_slices - 2 * np.pi, x_two_homogenous_slices, c=color2, s=scatter_size, alpha=transparency_analytic, cmap='viridis', zorder=1)
elif (Approximation == 'Off'):
    exit

# -----------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------
mpl.rcParams['font.family'] = "Latin Modern"
mpl.rcParams['font.serif'] = "Latin Modern Math"
#                      ----------------------------------------------------
if (NM == 0):
    #                      --------------- BM = 0.4, NM = 0.0 ------------
    rectangle = plt.Rectangle((x_min, 2.86),
                              abs(x_min) + abs(x_max), 0.64,
                              fc='grey',
                              alpha=0.15)
    plt.gca().add_patch(rectangle)


#                      -----------------------------------------------------
elif (NM == 0.2):
    #                      --------------- BM = 0.4, NM = 0.2 ------------
    rectangle = plt.Rectangle((x_min, 2.235),
                              abs(x_min), 0.625,
                              fc='grey',
                              alpha=0.15)
    # plt.gca().add_patch(rectangle)

    rectangle = plt.Rectangle((0, 3.49),
                              abs(x_max), 0.625,
                              fc='grey',
                              alpha=0.15)
    # plt.gca().add_patch(rectangle)
    #                      -----------------------------------------------------
else:
    exit

#                      -------------------Vertical Line---------------------

dotted_line = plt.Line2D((x_min, x_max), (0, 0), lw=3.,
                         ls='-', marker='.',
                         markersize=00,
                         markerfacecolor='black',
                         markeredgecolor='black',
                         color='Black',
                         alpha=0.7)
plt.gca().add_line(dotted_line)

if (Central_Vertical_Line == 'On'):
    dotted_line = plt.Line2D((0, 0), (y_min, y_max), lw=3.,
                             ls='-.', marker='.',
                             markersize=00,
                             markerfacecolor='black',
                             markeredgecolor='black',
                             color='Black',
                             alpha=0.7)
    plt.gca().add_line(dotted_line)
elif (Central_Vertical_Line == 'Off'):
    exit

if (Vertical_Lines == 'On'):
    MULTI_N = 10
    for i in range(-MULTI_N, MULTI_N, 1):
        set_points = i * 2.0 * np.pi - np.pi + 2.0 * np.pi * np.pi * (NM / K_M)
        dotted_line = plt.Line2D((set_points, set_points), (0, 6), lw=2.,
                                 ls='-.', marker='.',
                                 markersize=00,
                                 markerfacecolor='black',
                                 markeredgecolor='black',
                                 alpha=0.4)
        plt.gca().add_line(dotted_line)

        set_points_2 = i * 2.0 * np.pi - 2.0 * np.pi + 4.0 * np.pi * np.pi * (NM / K_M)
        dotted_line = plt.Line2D((set_points_2, set_points_2), (0, 6), lw=2.,
                                 ls='-.', marker='.',
                                 markersize=00,
                                 markerfacecolor='black',
                                 markeredgecolor='black',
                                 alpha=0.4)
        plt.gca().add_line(dotted_line)
elif (Vertical_Lines == 'Off'):
    exit

#                      -------------------Modulated Lines---------------------

if (NUMBER_OF_LINE_FIELD % 2) == 0:
    HALF_LINES_number = NUMBER_OF_LINE_FIELD / 2
else:
    HALF_LINES_number = (NUMBER_OF_LINE_FIELD - 1) / 2

HALF_LINE_FIELD = int(HALF_LINES_number)

for i in range(-HALF_LINE_FIELD, HALF_LINE_FIELD + 1, 1):
    POSITION_OF_LINE = middle_of_the_lines + (space_multiplier_between_the_lines * i * (2 / NUMBER_OF_LINE_FIELD))
    dotted_line = plt.Line2D((-2.0 * K_M, 2.0 * K_M), (POSITION_OF_LINE - (TILT) * K_M, POSITION_OF_LINE + (TILT) * K_M), lw=3.,
                             ls='-', marker='.',
                             markersize=00,
                             markerfacecolor='black',
                             markeredgecolor='black',
                             color="royalblue",
                             alpha=1.0)
    if (Modulated_Lines == 'On'):
        plt.gca().add_line(dotted_line)
    elif (Modulated_Lines == 'Vector'):
        print('Yo')
    elif (Modulated_Lines == 'Off'):
        print("##########!!! Attention: Mode 'Modulated Lines' is Off !!!##########")
        break

#                      -----------------------------------------------------

# -----------------------------------------------------------------------------------------

# ----------------------------------------Legends------------------------------------------

# plt.legend(loc='upper left', prop={'size': 20}, framealpha=1)

# -----------------------------------------------------------------------------------------

# ------------------------Printing Plots in a format (pdf, eps, etc.)----------------------

fig.set_size_inches(width, height)
fig.savefig('plot.pdf')

# -----------------------------------------------------------------------------------------

# _________________________________________________________________________________________
# -----------------------------------------------------------------------------------------
