function Save_DS_for_R(DataSet,FileName_base)
	% Saves spectral wavelengths, spectral intesities, other data 
	% from dataset "DataSet" to separate csv files. The Files share
	% common part of filename, indicated in "FileName_base"
	% 
	%   Save_DS_for_R(DataSet,FileName_base)
	%	
	%	To read these `.csv` files to `R` use function 
	%	`spHelper::read3csv2hy()`.
	% 
	% 2015-10-06
	
	try bru; end
	disp(FileName_base)
	disp('Saving to R format. Please, wait...')

	% Extract necessary variables
	% .y - variable with specroscopic data
	wavelengths         = DataSet.Properties.UserData.x;
	spectra_intensities = DataSet.y;

	Data   = DataSet;
	Data.y = [];
	VarNames = DataSet.Properties.VarNames';
	UserData = DataSet.Properties.UserData;

	% save variables to separate files
	% csvwrite([FileName_base ' (data VarNames).csv'], VarNames)
	csvwrite([FileName_base ' (spectra).csv'],       spectra_intensities)
	csvwrite([FileName_base ' (wavelengths).csv'],   wavelengths)

	export(Data,'File',[FileName_base ' (data).csv'],...
		'delimiter','|',...
		'WriteVarNames',true)

	% export(UserData,[FileName ' (information).csv'],...
	% 'delimiter',',','WriteVarNames',false)
	% whos spectra_intensities wavelengths Data
	disp('Saving to R format is completed')
	try bru; end
end


function bru(marker,mLength)
	if ~exist('marker','var')||isempty(marker); marker='_'; end;
	if ~exist('mLength','var'); mLength = 90; end;
	disp(repmat(marker,1,mLength))
end