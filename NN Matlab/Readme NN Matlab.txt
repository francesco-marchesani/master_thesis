[Readme NN MATLAB]

---

In this folder it is possible to find MATLAB code concerning:

- Neural network training and prediction script - (nn_script.m)

- Suitable MATLAB workspace for the script - (matlab_workspace_nn.mat)
	|-> with the following variables:
			* n = number of predictions to made
			* pn = vector of previous observations y(t)
			* predicted_data = data predicted with the NN
			* c = vector of previous observations + predicted data

- Additional NN worskapce with the generated net 
	|-> [d=10, hn=10] - (matlab_workspace_nn_additional.mat)
	* Note * predicted data are into the variable y2 after each execution of the nn_script!

---

Just import and run the files in a MATLAB environment with the NN Toolbox available.
A neural net will be generated (with additional MATLAB workspace variables)
