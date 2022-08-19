set -ex

cat custom_vars/custom_var

grep 'OTP_VERSION: 2[3-5]\.' custom_vars/custom_var
