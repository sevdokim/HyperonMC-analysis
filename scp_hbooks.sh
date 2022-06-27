if [ -z $1 ] ; then 
    echo "Usage: 

    ./scp_hbooks.sh PATTERN

    where PATTERN is part of MC production name"
    exit 1
fi
for file in $(ls *${1}*/calibr.hbook.bz2) ; do
    echo "scp $file evdokimov@alice5.ihep.su:/data3/users/evdokimov/hyperon/hbooks/$(echo $file | awk -F/ '{print $1".hbook.bz2"}') "   
    scp $file evdokimov@alice5.ihep.su:/data3/users/evdokimov/hyperon/hbooks/$(echo $file | awk -F/ '{print $1".hbook.bz2"}') ;
done
