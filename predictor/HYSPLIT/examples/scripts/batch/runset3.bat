@ECHO OFF

rem any line that starts with rem is only a remark, i.e., it is not executed
rem this is an MS-DOS batch file, that is run from the MS-DOS command line, by typing "set" and then return
rem or that can be called from another batch file
rem in this case, it is called from run.bat

rem example set of replaceable parameters

rem param 1 = file name
rem param 2 = emis rate


rem
rem **********************************************************************
rem
rem 4. Total Run Time for Simulation (hours)
rem    default = 72 in HYSPLIT_4
rem
ECHO      72    >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 5. Method for Estimating Vertical Motion
rem    default = 0 in HYSPLIT_4
rem
rem    0:data=model's vertical velocity fields;
rem    1:isobaric; 2:isentropic; 3:constant density; 4:constant sigma
rem
ECHO            0 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 6. Top of Model Domain (internal coordinates; meters above sea lvl)
rem    default = 10000.0 in HYSPLIT_4
rem
ECHO     10000.000000 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 7. Number of Input Data Grids
rem    Number of simultaneous meteorological fields to be input.
rem    The following two entries (directory and name) will be repeated
rem    this number of times. Always start with the finest grid as
rem    number #1. When the first puff or particle moves on to the
rem    next grid, all subsequent particles are automatically
rem    transferred to the new grid. There should be time and space
rem    overlap for multiple grids.
rem    default = 1 in HYSPLIT_4
rem
ECHO            1 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 8. Grid # 1 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
ECHO e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 9. Grid # 1 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
ECHO RP199501.gbl >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 10. Grid # 2 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
rem e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 11. Grid # 2 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
rem RP199502.gbl >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 12. Grid # 3 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
rem e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 13. Grid # 3 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
rem RP199503.gbl >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 14. Grid # 4 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
rem e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 15. Grid # 4 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
rem RP199504.gbl >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 16. Grid # 5 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
rem e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 17. Grid # 5 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
rem RP199505.gbl >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 18. Grid # 6 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
rem e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 19. Grid # 6 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
rem RP199506.gbl >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 20. Grid # 7 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
rem e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 21. Grid # 7 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
rem RP199507.gbl >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 22. Grid # 8 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
rem e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 23. Grid # 8 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
rem RP199508.gbl >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 24. Grid # 9 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
rem e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 25. Grid # 9 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
rem RP199509.gbl >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 26. Grid # 10 Directory
rem    Directory location of the meteorological files.
rem    Always terminate with the appropriate slash.
rem    Default: /main/sub/data/
rem
rem    If there is more than one grid, will have subsequent lines
rem    defining directories for each grid
rem
rem
rem e:\hysplit4\syracuse\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 27. Grid # 10 File Name. Name of the meteorological data file.
rem    Default: file_name
rem
rem RP199510.gbl >>CONTROL.DAT
rem
rem 28. Grid # 11 Directory                                               
rem    Directory location of the meteorological files.                    
rem    Always terminate with the appropriate slash.                       
rem    Default: /main/sub/data/                                           
rem                                                                       
rem    If there is more than one grid, will have subsequent lines         
rem    defining directories for each grid                                 
rem                                                                       
rem                                                                       
rem e:\hysplit4\syracuse\ >>CONTROL.DAT                                                       
rem                                                                       
rem **********************************************************************
rem                                                                       
rem 29. Grid # 11 File Name. Name of the meteorological data file.        
rem    Default: file_name                                                 
rem                                                                       
rem RP199511.gbl >>CONTROL.DAT                                                       
rem                                                                       
rem 30. Grid # 12 Directory                                               
rem    Directory location of the meteorological files.                    
rem    Always terminate with the appropriate slash.                       
rem    Default: /main/sub/data/                                           
rem                                                                       
rem    If there is more than one grid, will have subsequent lines         
rem    defining directories for each grid                                 
rem                                                                       
rem                                                                       
rem e:\hysplit4\syracuse\ >>CONTROL.DAT                                                       
rem                                                                       
rem **********************************************************************
rem                                                                       
rem 31. Grid # 12 File Name. Name of the meteorological data file.        
rem    Default: file_name                                                 
rem                                                                       
rem RP199512.gbl >>CONTROL.DAT                                                       
rem                                                                       
rem **********************************************************************
rem ######################################################################
rem **********************************************************************
rem              SECTION 2: POLLUTANT DEFINITION ENTRIES
rem
rem   if running for more than one pollutant at a time, this
rem   this section has to be repeated for each pollutant 
rem
rem **********************************************************************
rem ######################################################################
rem **********************************************************************
rem
rem 28. Number of different pollutants
rem     Multiple pollutant species may be defined for emission.
rem     Each species may behave differently for deposition calculations.
rem     Each will be tracked on its own concentration grid. The following
rem     four entries are repeated for each pollutant defined.
rem     Default: 1
rem
ECHO            1 >>CONTROL.DAT
rem
rem **********************************************************************
rem       emissions data for Hg(0)
rem **********************************************************************
rem
rem 29(1)- Pollutant four Character Identification
rem    Any four character label that can be used to identify the
rem    pollutant. The label is written with the concentration output
rem    grid to identify output records associated with that pollutant
rem    and will appear in display labels. In addition optional
rem    deposition and chemistry calculations may be keyed to that
rem    identification string.
rem    Default: TEST
rem
ECHO SO2g >>CONTROL.DAT  
rem
rem **********************************************************************
rem
rem 30(2)- Emission rate (per hour)
rem    Mass units released each hour. Units are arbitrary except when
rem    specific chemical transformation subroutines are attached to the
rem    calculation. Output air concentration units will be in the same
rem    units as specified on this line. i.e., input kg/hr -> output kg/m,
rem    input Bq/hr -> output Bq/m ) 3 3
rem    Default: 1.0
rem
ECHO         %2 >>CONTROL.DAT
rem
rem *********************************************************************
rem
rem 31(3)- Hours of emission
rem    The duration of emission may be defined in fractional hours.
rem    Durations of less than one time-step will be emitted over one
rem    time-step, but with the correct total emission requested
rem    (rate x duration).
rem    Default: 1.0
rem
rem   will have 1 hour of emissions (every 7 hours in standard runs)
ECHO         72.000000 >>CONTROL.DAT    
rem
rem *********************************************************************
rem
rem 32(4)- Release start time: year month day hour minute
rem    The previously specified hours of emission starts at this time.
rem    An entry of zero's in the field, when input is read from a file,
rem    will also result in the selection of the default values.
rem    Default: [simulation start time]
rem
ECHO      0        0        0        0        0 >>CONTROL.DAT                           
rem
rem **********************************************************************
rem ######################################################################
rem **********************************************************************
rem            SECTION 3: CONCENTRATION GRID DEFINITION
rem   Dispersion calculations are performed without regard to any
rem   specific concentration grid, although the grid spacing may
rem   affect the model's integration time step. Normally one would
rem   use this section to define a grid system for postprocessing
rem   and display of the model's output over the computational domain.
rem **********************************************************************
rem ######################################################################
rem **********************************************************************
rem
rem 33- Number of simultaneous concentration grids
rem     Multiple or nested grids may be defined. The concentration output
rem     grids are treated independently. The following 10 entries will be
rem     repeated for each grid defined. The number of grids permitted
rem     depends upon model compilation parameters.
rem     Default: 1
rem
ECHO            1 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 34(1)- Center Latitude, Longitude (degrees)
rem    The center position of the concentration sampling grid.
rem    Zero's from an input file will result in default values.
rem    Default: [source location]
rem
ECHO     44.0   -71.0  >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 35(2)- Grid spacing (degrees) Latitude, Longitude
rem    The interval in degrees between nodes of the sampling grid.
rem    Puffs must pass over a node to contribute concentration to that
rem    point and therefore if the spacing is too wide they may pass
rem    between intersection points. Particle model calculations
rem    represent grid-cell averages, where each cell is centered on a
rem    node position, with its dimensions equal to the grid spacing.
rem    Finer resolution concentration grids require correspondingly
rem    finer time-steps. This may be mitigated to some extent by
rem    limiting fine resolution grids to only the first few hours
rem    of the simulation.
rem    Default: 1.0 1.0
rem
rem
ECHO     0.5             0.5          >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 36(3)- Grid span (degrees) Latitude, Longitude
rem    The total span of the grid in each direction. For instance,
rem    a span of 10 degrees would cover 5 degrees on each side of
rem    the center grid location. A default span is computed from
rem    the compiled maximum dimensions of the concentration grid
rem    divided by the grid spacing requested in the previous entry.
rem    Larger spans would require recompilation.
rem    Default: [max_Y_pnts / delta_lat] [max_X_pnts / delta_long]
rem
ECHO        10.000000       10.000000 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 37(4)- Enter grid # 1 directory
rem
rem    Directory to which the binary gridded concentration output
rem    file for this grid is written. As in other directory entries
rem    a terminating separator is required.
rem    Default: /main/sub/output/
rem
ECHO e:\hysplit4\ohiojan\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 38(5)- Enter grid # 1 file name
rem    Name of the concentration output file for each grid. See
rem    Section 6 for a description of the format of the concentration
rem    output file.
rem    Default: file_name
rem
ECHO %1 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 39(6)- Number of vertical concentration levels
rem    The number vertical levels in the concentration grid. If
rem    deposition is defined output to the ground is considered a level.
rem    Default: 1
rem
ECHO            2 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 40(7)- Height of each level m AGL
rem    Output grid levels may be defined in any order for the puff
rem    model as long as the deposition level (0) comes first; a
rem    height of zero indicates deposition output. Air concentrations
rem    must have a non-zero height defined. A height for the puff
rem    model indicates the concentration at that level. A height for
rem    the particle model indicates the average concentration between
rem    that level and the previous level (or the ground for the first
rem    level). Therefore heights for the particle model need to
rem    be defined in ascending order.
rem    Default: 50
rem
ECHO     0  50 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 41(8)- Sampling start time: year month day hour minute
rem    Each concentration grid may have a different starting, stopping,
rem    and output averaging time. Zero entry from a file will also
rem    result in default values.
rem    Default: [simulation start time]
rem
ECHO            0           0           0           0           0 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 42(9)- Sampling stop time: year month day hour minute
rem    After this time no more concentration records are written.
rem    Early termination of high resolution grids (after the plume
rem    has moved away from the source) is an effective way of speeding
rem    up the computation if high resolution output is required. Once
rem    the output is terminated that particular grid resolution is no
rem    longer used for time-step computations.
rem    Default: 99 12 31 24 60
rem
ECHO            0           0           0           0           0 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 43(10)- Sampling interval: type hour minute
rem    Each grid may have its own sampling or averaging interval.
rem    The interval can be of two different types: averaging (type=0)
rem    or snapshot (type=1). Averaging will produce output averaged
rem    over the specified interval. Snapshot will give the instantaneous
rem    output at the output interval. For instance you may want to
rem    define a concentration grid that produces 24 hour average air
rem    concentrations for the duration of the simulation which for
rem    the default case of a 2 day simulation will result in 2 output
rem    maps, one for each day. Each defined grid can have a different
rem    output type and interval.
rem    Default: 0 24 0
rem
ECHO            0           72           0 >>CONTROL.DAT
rem
rem
rem 34(1)- Center Latitude, Longitude (degrees)
rem    The center position of the concentration sampling grid.
rem    Zero's from an input file will result in default values.
rem    Default: [source location]
rem
rem     44.0   -71.00  >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 35(2)- Grid spacing (degrees) Latitude, Longitude
rem    The interval in degrees between nodes of the sampling grid.
rem    Puffs must pass over a node to contribute concentration to that
rem    point and therefore if the spacing is too wide they may pass
rem    between intersection points. Particle model calculations
rem    represent grid-cell averages, where each cell is centered on a
rem    node position, with its dimensions equal to the grid spacing.
rem    Finer resolution concentration grids require correspondingly
rem    finer time-steps. This may be mitigated to some extent by
rem    limiting fine resolution grids to only the first few hours
rem    of the simulation.
rem    Default: 1.0 1.0
rem
rem
rem     0.5             0.5          >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 36(3)- Grid span (degrees) Latitude, Longitude
rem    The total span of the grid in each direction. For instance,
rem    a span of 10 degrees would cover 5 degrees on each side of
rem    the center grid location. A default span is computed from
rem    the compiled maximum dimensions of the concentration grid
rem    divided by the grid spacing requested in the previous entry.
rem    Larger spans would require recompilation.
rem    Default: [max_Y_pnts / delta_lat] [max_X_pnts / delta_long]
rem
rem        7.000000       10.000000 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 37(4)- Enter grid # 1 directory
rem
rem    Directory to which the binary gridded concentration output
rem    file for this grid is written. As in other directory entries
rem    a terminating separator is required.
rem    Default: /main/sub/output/
rem
rem e:\hysplit4\concmdl\ >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 38(5)- Enter grid # 1 file name
rem    Name of the concentration output file for each grid. See
rem    Section 6 for a description of the format of the concentration
rem    output file.
rem    Default: file_name
rem
rem %1 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 39(6)- Number of vertical concentration levels
rem    The number vertical levels in the concentration grid. If
rem    deposition is defined output to the ground is considered a level.
rem    Default: 1
rem
rem            2 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 40(7)- Height of each level m AGL
rem    Output grid levels may be defined in any order for the puff
rem    model as long as the deposition level (0) comes first; a
rem    height of zero indicates deposition output. Air concentrations
rem    must have a non-zero height defined. A height for the puff
rem    model indicates the concentration at that level. A height for
rem    the particle model indicates the average concentration between
rem    that level and the previous level (or the ground for the first
rem    level). Therefore heights for the particle model need to
rem    be defined in ascending order.
rem    Default: 50
rem
rem     0  50 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 41(8)- Sampling start time: year month day hour minute
rem    Each concentration grid may have a different starting, stopping,
rem    and output averaging time. Zero entry from a file will also
rem    result in default values.
rem    Default: [simulation start time]
rem
rem            0           0           0           0           0 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 42(9)- Sampling stop time: year month day hour minute
rem    After this time no more concentration records are written.
rem    Early termination of high resolution grids (after the plume
rem    has moved away from the source) is an effective way of speeding
rem    up the computation if high resolution output is required. Once
rem    the output is terminated that particular grid resolution is no
rem    longer used for time-step computations.
rem    Default: 99 12 31 24 60
rem
rem            0           0           0           0           0 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 43(10)- Sampling interval: type hour minute
rem    Each grid may have its own sampling or averaging interval.
rem    The interval can be of two different types: averaging (type=0)
rem    or snapshot (type=1). Averaging will produce output averaged
rem    over the specified interval. Snapshot will give the instantaneous
rem    output at the output interval. For instance you may want to
rem    define a concentration grid that produces 24 hour average air
rem    concentrations for the duration of the simulation which for
rem    the default case of a 2 day simulation will result in 2 output
rem    maps, one for each day. Each defined grid can have a different
rem    output type and interval.
rem    Default: 0 24 0
rem
rem            0           24           0 >>CONTROL.DAT
rem
rem **********************************************************************
rem ######################################################################
rem **********************************************************************
rem                 SECTION 4: DEPOSITION DEFINITIONS
rem **********************************************************************
rem ######################################################################
rem **********************************************************************
rem
rem 44- Number of pollutants depositing
rem     Deposition parameters must be defined for each pollutant species
rem     emitted. Each species may behave differently for deposition
rem     calculations. Each will be tracked on its own concentration
rem     grid. The following five lines are repeated for each pollutant
rem     defined. Currently the number here must be identical to the
rem     number on line 10. Deposition is turned off for pollutants by
rem     an entry of zero in all fields.
rem     Default: number of pollutants defined on line # 10
rem
ECHO            1 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 45(1)- Particle: Diameter (um), Density (g/cc), and Shape
rem    These three entries are used to compute gravitational
rem    settling for particles. A value of zero in any field will cause
rem    the pollutant to be considered as a gas. All three fields must
rem    be defined for particle deposition calculations. These values
rem    need to be correct only if gravitational settling is to be
rem    computed by the model, otherwise a nominal value of one may be
rem    assigned as a default for each entry, and a dry deposition
rem    velocity should be specified in the next line. If wet removal
rem    is required for particles, then any non-zero value in all fields
rem    will define a particle for wet deposition calculations.
rem
rem    Default: 0.0 0.0 0.0
rem    Suggested for Cs-137: 1.0 1.0 1.0
rem
ECHO   0.0   0.0   0.0 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 46(2)- Deposition velocity (m/s),
rem        Pollutant molecular weight (Gram/Mole),
rem        Surface Reactivity Ratio,
rem        Diffusivity Ratio,
rem        Effective Henry's Constant
rem
rem        Dry deposition calculations are performed at the lowest
rem        model level based upon the relation that the deposition
rem        flux equals the velocity times the ground-level air
rem        concentration. This calculation is available for gases
rem        and particles. However specification of a non-zero dry
rem        deposition velocity will take precedence over gravitational
rem        settling or the computation of a dry deposition velocity
rem        based upon land-use which requires setting the remaining
rem        four parameters (molecular weight, surface reactivity,
rem        diffusivity, and effective Henry's constant). The
rem        calculation of the dry deposition velocity uses the
rem        resistance method. See the discussion about dry deposition
rem        in the model equations reference before setting these values.
rem        The most simple option is to define a default value for the
rem        deposition velocity. Although this value over-rides the
rem        gravitational settling computed from the definition of a
rem        particle on line 27, non-zero values on line 27 are the
rem        only way to define a particle for wet removal calculations.
rem        Default: 0.0 0.0 0.0 0.0 0.0
rem        Suggested Dry Deposition: Cs-137 0.001
rem
ECHO   0.0   64    0.0    1.9    1.0E+05 >>CONTROL.DAT
rem
rem
rem **********************************************************************
rem
rem 47(3)- Wet Removal: Henry's (Molar/atm),
rem                     In-cloud (L/L), Below-cloud (1/s)
rem        Henry's constant defines the wet removal process for
rem        soluble gases. It is defined only as a first-order process
rem        by a non-zero value in the field. Wet removal of particles
rem        is defined by non-zero values for the in-cloud and below-cloud
rem        parameters. In-cloud removal is defined as a ratio of pollutant
rem        in cloud-water to that in rain. Below-cloud removal is defined
rem        through a removal time constant. One or both processes may be
rem        defined for particles.
rem
rem              NOTE: I think that the in-cloud parameter is really the 
rem              ratio of pollutant in the overall cloud (grams/liter of cloud)
rem              to that in rain (grams/liter of rain). In HYSPLIT_3, I had
rem              changed to mass based units (grams of pollutant per gram
rem              of cloud divided by grams of pollutant per gram of rain),
rem              and the value of 3.2x10^5 changed to 400 in these units.
rem
rem              Subsequently, I changed the washout ratio from 400 to 50,
rem              in an attempt to better match measurement data for dioxin.
rem
rem              The "50" value corresponds to a value of 4x10^4 in the 
rem              volume-based units... 
rem
rem        Default: 0.0 0.0 0.0
rem        Suggested for Cs-137: 0.0 3.2x10^5 5x10^-5
rem
ECHO   1.24    0    0 >>CONTROL.DAT 
rem
rem **********************************************************************
rem
rem 48(4)- Radioactive decay half-life (days)
rem    A non-zero value in this field initiates the decay process of
rem    both airborne and deposited pollutants.
rem    Default: 0.0
rem    Suggested for Cs-137: 11000
rem
ECHO     0.000000E+00 >>CONTROL.DAT
rem
rem **********************************************************************
rem
rem 49(5)- Pollutant Resuspension (1/m)
rem    A non-zero value causes deposited pollutants to be re-emitted
rem    based upon soil conditions, wind velocity, and particle type.
rem    It is parameterized based upon a resuspension factor. Pollutant
rem    resuspension requires the definition of a deposition grid, as
rem    the pollutant is re-emitted from previously deposited material.
rem    Under most circumstances the deposition should be accumulated on
rem    the grid for the entire duration of the simulation. Air
rem    concentrations may be defined at finer temporal scales.
rem    Default: 0.0
rem    Suggested : 10^6
rem
ECHO     0.000000E+00 >>CONTROL.DAT
rem
rem
rem **********************************************************************
rem
rem    NOW THAT ALL INPUTS HAVE BEEN SET, DO SIMULATION
rem
rem **********************************************************************
rem
del control.
copy control.dat control.
