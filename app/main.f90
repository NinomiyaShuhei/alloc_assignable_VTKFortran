program main
    use, intrinsic :: iso_fortran_env
    use :: vtk_fortran, only : vtk_file
    implicit none
    type(vtk_file)                :: a_vtk_file                                                   !< A VTK file.
    integer(int32), parameter       :: np = 27_int32                                                  !< Number of points.
    integer(int32), parameter       :: nc = 11_int32                                                  !< Number of cells.
    real(real32), allocatable       :: x(:)                                                           !< X coordinates of points.
    real(real32), allocatable       :: y(:)                                                           !< Y coordinates of points.
    real(real32), allocatable       :: z(:)                                                           !< Z coordinates of points.
    integer(int8), allocatable :: cell_type(:)      !< Cells type.
    integer(int32), allocatable :: offset(:)       !< Cells offset.
    integer(int32), allocatable :: connect(:)                                                      !< Connectivity.
    real(real64),    allocatable :: v(:)                                                            !< One component points-variable.
    integer(int32), allocatable :: v_x(:)                                                          !< X component points-variable.
    integer(int32), allocatable :: v_y(:)                                                          !< Y component points-variable.
    integer(int32), allocatable :: v_z(:)                                                          !< Z component points-variable.
    integer(int32)                  :: error                                                        !< Status error.
    logical                       :: test_passed(1)                                               !< List of passed tests.

    x = [0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2]
    y = [0,0,0,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
    z = [0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6]

    cell_type = [12_int8,12_int8,10_int8,10_int8,7_int8,6_int8,9_int8,5_int8,5_int8,3_int8,1_int8]
    offset = [8_int32,16_int32,20_int32,24_int32,30_int32,36_int32,40_int32,43_int32,46_int32,48_int32,49_int32]

    connect = [0 ,1 ,4 ,3 ,6 ,7 ,10,9 , &
            1 ,2 ,5 ,4 ,7 ,8 ,11,10, &
            6 ,10,9 ,12,             &
            5 ,11,10,14,             &
            15,16,17,14,13,12,       &
            18,15,19,16,20,17,       &
            22,23,20,19,             &
            21,22,18,                &
            22,19,18,                &
            26,25,                   &
            24]
    v = [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0, &
        18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0]
    v_X = [1,1,0,1,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    v_Y = [0,1,2,0,1,2,0,1,2,0,1,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    v_Z = [0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
    ! ascii
    error = a_vtk_file%initialize(format='ascii', filename='XML_UNST-ascii.vtu', mesh_topology='UnstructuredGrid')
    call write_data
    error = a_vtk_file%finalize()
    ! raw
    error = a_vtk_file%initialize(format='raw', filename='XML_UNST-raw.vtu', mesh_topology='UnstructuredGrid')
    call write_data
    error = a_vtk_file%finalize()
    ! binary
    error = a_vtk_file%initialize(format='binary', filename='XML_UNST-binary.vtu', mesh_topology='UnstructuredGrid')
    call write_data
    error = a_vtk_file%finalize()

    test_passed = .true. ! nothing to test yet

    print "(A,L1)", new_line('a')//'Are all tests passed? ', all(test_passed)
    stop
contains
    subroutine write_data
        !< Write data.

        error = a_vtk_file%xml_writer%write_piece(np=np, nc=nc)
        error = a_vtk_file%xml_writer%write_geo(np=np, nc=nc, x=x, y=y, z=z)
        error = a_vtk_file%xml_writer%write_connectivity(nc=nc, connectivity=connect, offset=offset, cell_type=cell_type)
        error = a_vtk_file%xml_writer%write_dataarray(location='node', action='open')
        error = a_vtk_file%xml_writer%write_dataarray(data_name='scalars', x=v)
        error = a_vtk_file%xml_writer%write_dataarray(data_name='vector', x=v_x, y=v_y, z=v_z)
        error = a_vtk_file%xml_writer%write_dataarray(location='node', action='close')
        error = a_vtk_file%xml_writer%write_piece()
    endsubroutine write_data
end program main